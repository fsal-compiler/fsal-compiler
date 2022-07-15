[<Microsoft.FSharp.Core.AutoOpen>]
module Fs.AL.Compiler.IntermediateLanguage.ALTypeMapping

open System
open System.Collections.Generic
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols

open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract
open Fs.AL.Compiler.Fullname

let private (|HasALType|_|) (s:FSharpType) =
    match s.TypeDefinition |> FSharpEntity.tryGetALObjectKind with
    | Some alType -> alType |> Some
    | None -> None

module ALType =
    type private t = Fs.AL.Core.ALCoreValues.ALType
    
    let rec ofFSharpType (ftype:FSharpType) =
        let rootType = ftype |> FSharpType.getRootType
        let fullname = rootType |> FSharpType.getFullName
        
        
        match rootType.TypeDefinition.IsProvidedAndErased with
        | true ->
            if fullname.StartsWith "Fable.JsonProvider.Generator<...>" then Simple JsonToken
            else failwith $"unknown typeprovider type {fullname}"
        | _ ->  
        
        
        
        match rootType with
        | HasALType alType -> alType
        | ALTypeReplacements.HasSupportedCoreLibType alType -> alType  
        | _ -> 
        
        let debug = 1
        
        match fullname with
        | "Fs.AL.Core.ALSimpleValues.ALJsonObject" -> Simple JsonObject
        | "Fs.AL.Core.ALSimpleValues.ALJsonToken" -> Simple JsonToken
        | "Fable.JsonProvider.Generator<...>" -> Simple JsonToken
        | Operators.ref -> ftype.GenericArguments[0] |> ofFSharpType
        | "Microsoft.FSharp.Core.byref" -> ftype.GenericArguments[0] |> ofFSharpType
        | "Microsoft.FSharp.Core.Ref" -> ftype.GenericArguments[0] |> ofFSharpType
        // array types
        | "Microsoft.FSharp.Core.[]" ->
            let arrayof = ftype.GenericArguments[0] |> ofFSharpType
            match arrayof with
            | Simple JsonToken -> Simple JsonArray
            | Simple (Char) -> Simple (ALSimpleType.List (ALSimpleType.Char))
            | Simple (Integer) -> Simple (ALSimpleType.List (ALSimpleType.Integer))
            | Simple (Decimal) -> Simple (ALSimpleType.List (ALSimpleType.Decimal))
            | Simple (Text n) -> Simple (ALSimpleType.List (ALSimpleType.Text n))
            | _ -> failwith "unimplemented"
        | "System.Collections.Generic.List" ->
            let listof = ftype.GenericArguments[0] |> ofFSharpType
            match listof with
            | Simple JsonToken -> Simple JsonArray
            | Simple (st) -> Simple (List st)
            | x -> failwith $"invalid AL list, use an array? %A{x}"
        | "System.Collections.Generic.List`1.Enumerator" ->
            // todo: create seq type
            let listof = ftype.GenericArguments[0] |> ofFSharpType
            match listof with
            | Simple JsonToken -> Simple JsonArray
            | Simple (st) -> Simple (List st)
            | x -> failwith $"invalid AL list, use an array? %A{x}"
        // types inherited from type system
        | x when ftype |> FSharpType.hasBaseType<ALCode> -> Simple (Code None)
        | x when ftype |> FSharpType.hasBaseType<ALCodeunit> ->
            let name = ftype.TypeDefinition |> FSharpEntity.getALCompiledName
            Complex (Codeunit name)
        
        | x when ftype |> FSharpType.hasBaseType<ALRecord> ->
            let name = ftype.TypeDefinition |> FSharpEntity.getALCompiledName
            Complex (Record name)
        | x when ftype |> FSharpType.hasBaseType<ALSimpleValue> -> Simple (SimpleType (FSharpEntity.getALCompiledName ftype.TypeDefinition))
        | x when ftype |> FSharpType.hasBaseType<ALComplexValue> -> Complex (ComplexType (FSharpEntity.getALCompiledName ftype.TypeDefinition))
        | x when ftype.TypeDefinition |> FSharpEntity.hasAttribute<AL.Json> -> Simple JsonToken // use as json type
        | x when ftype.TypeDefinition |> FSharpEntity.hasAttribute<AL.Option> ->
            // cast cases to option types
            let cases =
                ftype.TypeDefinition.UnionCases
                |> Seq.map (fun f -> f.Name)
                |> Seq.toList
            Simple (Option cases) 
        // todo: handle union type fields too
//        | x when ftype.TypeDefinition.IsFSharpUnion ->
//            let unioncase = ftype.TypeDefinition.UnionCases
//            Simple JsonToken // use as json type
        // System.** namespace types
        | ALTypeReplacements.HasCoreLibType mapping -> mapping
        // jsonprovider types
        
        | x ->
            raise (NotImplementedException(x))
        
//        | x when ftype.BaseType.Value.TypeDefinition.FullName = FSharpType.ALTypeFullNames.record ->
//            let roottype = ftype.TypeDefinition |> FSharpEntity.getRootEntity
//            let name = ftype.TypeDefinition.DisplayName
//            Complex (Record (roottype |> FSharpEntity.getALCompiledName))
        // just for debugging
//        | x -> failwith $"unsupported type {x}"
        
            
    let isRef (fst:FSharpType) = fst.TypeDefinition |> FSharpType.isRefType
       
    let quotechars = ".+-$&[]/\\*\"`' " |> Seq.toArray
    let toString (x:t) =
        let d = 5
        match x with
        | Complex t ->
            match t with
            | ComplexType typename -> typename
            | Variant -> nameof Variant
            | Record name -> name
            | Codeunit name -> name
//                match name |> Seq.exists (fun f -> quotechars |> Array.contains f )
//                with
//                | true ->  $"Codeunit \"{name}\""
//                | false -> $"Codeunit {name}"
            | x ->
                let d = 5
                failwithf $"%A{x}"
        | Simple t ->
            match t with
            | Text lenOpt -> nameof Text
            | DateTime -> nameof DateTime
            | Integer -> nameof Integer
            | Boolean -> nameof Boolean
            | Decimal -> nameof Decimal
            | Code lenOpt -> nameof Code
            | JsonToken -> nameof JsonToken
            | JsonArray -> nameof JsonArray
            | JsonObject -> nameof JsonObject
            | List (listparam) ->
                let lt = 5
                "ListOf:" + listparam.ToString()
            | Option cases ->
                "OptionOf:" + cases.ToString()
            | SimpleType typename ->
                match typename with
//                | "JsonToken" -> "JsonToken"
//                | "JsonArray" -> "JsonArray"
//                | "JsonObject" -> "JsonObject"
//                | "JsonValue" -> "JsonValue"
                | "XmlDocument" -> "XmlDocument"
                | "XmlNode" -> "XmlNode"
                | "XmlNodeList" -> "XmlNodeList"
//                | "FUNCTION" -> "TEST TEST TEST"
                | _ -> 
                    let d = 5
                    failwithf $"%A{x}"
            | x ->
                let d = 5
                failwithf $"%A{x}"
        

    let oa : int64 = 1
    
    
module ALVariable =
    
    let createSimple name (simpleType:ALSimpleType) =
        {
            name = name
            isMutable = false
            altype = Simple simpleType
        }
        
    let createComplex name (complexType:ALComplexType) =
        {
            name = name
            isMutable = false
            altype = Complex complexType
        }
        
    let createForMfv (mfv:FSharpMemberOrFunctionOrValue) =
        {
            name = mfv.DisplayName
            isMutable = false
            altype = ALType.ofFSharpType mfv.FullType
        }
        
    let createStaticModule (entity:FSharpEntity) refType =
        let dispName = "@s@"+ entity.DisplayName
        {
            name = dispName
            isMutable = false
            altype = refType
        }
    
    /// compiler generated json token
    let createGenJsonToken (target:string) (targetJsonProp:string) (localvars:ICollection<ALVariable>) =
        let intermediaryVarName = $"@j@{target}.{targetJsonProp}_{localvars.Count}"
        {   name = intermediaryVarName
            isMutable = false
            altype = Simple JsonToken }
        
    let createGenField target targetProp alType (localvars:ICollection<ALVariable>) =
        let targetVarName = $"@{target}.{targetProp}_{localvars.Count}"
        {   name = targetVarName
            isMutable = false
            altype = alType }

    
    let private addIfNotExists (vars:ICollection<ALVariable>) name ifNot =
        let alVariableOption = 
            vars
            |> Seq.tryFind (fun f -> f.name = name)
        match alVariableOption with  
        | Some _ -> ()
        | None -> vars.Add(ifNot)

    let ensureHasJTokenVariable (vars:ICollection<ALVariable>) =
        addIfNotExists vars "@jtoken"
            {
              isMutable = false
              name= "@jtoken"
              altype = Simple JsonToken
            }