module rec Fs.AL.Compiler.CompilerSymbols

open System
open System.Runtime.Serialization
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.Fullname
open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract

type FSharpMember = (FSharpMemberOrFunctionOrValue * FSharpMemberOrFunctionOrValue list list * FSharpExpr)

    
module FSharpAttribute =
    let isOfType (t:Type) (attr:FSharpAttribute) =
        attr.AttributeType.FullName = t.FullName.Replace("+",".")
    
    
module FSharpSymbol =
    type private t = FSharpSymbol
    let (|IsTypeCast|_|) (x:t) =
        match x.FullName with
        | Operators.int -> Some()
        | Operators.string -> Some()
        | _ -> None
    let getTypeFullName (x:t) =
        match x with
        | :? FSharpEntity as v ->
            let roottype =
                v.AbbreviatedType
                |> FSharpType.getRootType
            roottype.TypeDefinition.FullName
        | :? FSharpMemberOrFunctionOrValue as v ->
            v.FullName
        | n -> raise (NotImplementedException($"symbol not implemented {n}"))
        
    let getRootType (x:t) =
        match x with
        | :? FSharpEntity as v ->
            let roottype =
                v.AbbreviatedType
                |> FSharpType.getRootType
            roottype            
        | n -> raise (NotImplementedException($"symbol not implemented {n}"))
        
    
//    targetType.GenericArguments[0].TypeDefinition
module FSharpType =
    
    type private t = FSharpType
    
    let tryGetGenericTypeArg (x:t) =
        match x.GenericArguments |> Seq.toList with
        | [ x ] -> Some x 
        | _ -> None 
        
        
    
    let getRootType (x:t) : FSharpType =
        let rec typeSeq (currType:t) =
            seq {
                yield currType
                yield! typeSeq currType.AbbreviatedType 
            }
        let rootType =
            typeSeq x 
            |> Seq.pairwise 
            |> Seq.find (fun (t1,t2) -> t1 = t2 )
            |> snd
        rootType
            
        
    
    let rec hasBaseType<'t> (x:t) =
        if x.IsFunctionType then false else
        let fullname = typeof<'t>.FullName.Replace("+",".")
        match x.TypeDefinition.TryFullName with
        | None -> false
        | Some fname ->
            if fname = fullname then true
            else
                match x.BaseType with
                | None -> false 
                | Some basetype -> basetype |> hasBaseType<'t>
            
    [<RequireQualifiedAccess>]
    module ALTypeFullNames =
        let record = typeof<ALRecord>.FullName.Replace("+",".")
        let codeunit = typeof<ALCodeunit>.FullName.Replace("+",".")
        let object = typeof<ALObject>.FullName.Replace("+",".")
        let simplevalue = typeof<ALSimpleValue>.FullName.Replace("+",".")
        let complexvalue = typeof<ALComplexValue>.FullName.Replace("+",".")
        
            
    /// compares type by fullname
    let isOfType<'t> (x:t) =
        match x.IsAbbreviation with
        | true ->
            let fullname1 = x.AbbreviatedType.TypeDefinition.FullName
            let fullname2 = typeof<'t>.FullName
            fullname1 = fullname2
        | false ->
            let fullname1 = x.AbbreviatedType.TypeDefinition.FullName
            let fullname2 = typeof<'t>.FullName.Replace("+",".")
            fullname1 = fullname2
    let rec isInheritedFromALType (ent:FSharpType option) =
        match ent with
        | None ->  false
        | Some value ->
            match value.TypeDefinition.TryFullName with
            | None -> false
            | Some fname -> 
                if fname = ALTypeFullNames.codeunit then true
                elif fname = ALTypeFullNames.record then true
                elif fname = ALTypeFullNames.simplevalue then true
                elif fname = ALTypeFullNames.complexvalue then true
                else value.TypeDefinition.BaseType |> isInheritedFromALType
                
    let isDirectlyInheritedFromALType (ent:FSharpType option) =
        match ent with
        | None ->  false
        | Some value ->
            match value.TypeDefinition.TryFullName with
            | None -> false
            | Some fname -> 
                if fname = ALTypeFullNames.codeunit then true
                elif fname = ALTypeFullNames.record then true
                elif fname = ALTypeFullNames.simplevalue then true
                elif fname = ALTypeFullNames.complexvalue then true
                else false
        
    let isRefType (x:FSharpSymbol) =
//        symbol.FullName = "Microsoft.FSharp.Core.FSharpRef`1"
        x.FullName = "Microsoft.FSharp.Core.byref"
        || x.FullName = "Microsoft.FSharp.Core.Ref"
    let isUnit (x:FSharpType) = x.AbbreviatedType.TypeDefinition.FullName = "Microsoft.FSharp.Core.Unit"
    
    let getRootFullName (x:FSharpType) =
        x |> FSharpType.getRootType |> FSharpType.getFullName
    let getFullName (x:FSharpType) =
        let t = 
            x.TypeDefinition
            :> FSharpSymbol
            |> (fun f -> f.FullName)
#if DEBUG
        match t with
        | "Microsoft.FSharp.Core.[]" -> () 
        | "Microsoft.FSharp.Core.Unit" -> ()
        | "Microsoft.FSharp.Core.Ref" -> ()
        | "Microsoft.FSharp.Core.byref" -> () 
        | _ when t.StartsWith("Microsoft.FSharp") ->  failwith $"invalid type {t}"
        | _ -> ()
#endif
        t
        
module FSharpEntity =
    
    type private t = FSharpEntity
    
    /// goes through abbreviations
    let rec getRootEntity (x:t) : FSharpEntity =
        let rt = 5
//        if x.Assembly.QualifiedName = "" then "+++JsonProvider:"+x.DisplayName else
        //TODO : test
        
        match x.IsFSharpAbbreviation with
        | false -> x
        | true -> x.AbbreviatedType.TypeDefinition |> getRootEntity
    
        
    let rec private getRootTypeFullName (x:t) =
        //TODO : there might be other provided types
        if x.IsProvided then
            let debug = 123123
            "System.Text.Json.Nodes.JsonValue" else
//        if x.IsArrayType then "System.Text.Json.Nodes.JsonArray" else
        match x.IsFSharpAbbreviation with
        | false -> x.FullName
        | true -> x.AbbreviatedType.TypeDefinition |> getRootTypeFullName
        
    let getFullName (x:t) =
        let rootfullname = x |> getRootTypeFullName
        rootfullname
    let getUninitializedInstance (x:t) =
        x.QualifiedName
        |> Type.GetType
        |> FormatterServices.GetUninitializedObject
    let getInstance (x:t) =
        x.QualifiedName
        |> Type.GetType
        |> Activator.CreateInstance
        
    /// gets first generic type arg
    let tryGetGenericTypeArg (x:t) =
        let ctor =
            x.MembersFunctionsAndValues
            |> Seq.tryFind (fun f -> f.IsConstructor )
            |> Option.bind (fun f -> f.FullTypeSafe )
            |> Option.map (fun f -> f.GenericArguments[1] )
        ctor
        
//    let isALObject (x:t) =
//        match x.BaseType with
//        | None -> false
//        | Some basetype ->
//            basetype |> FSharpType.isOfType<ALCodeunit>
//            || basetype |> FSharpType.isOfType<ALRecord>
          
    /// strongly typed comparison to runtime
    let isOfType (typ:Type) (x:t) =
        let symbolfullname = x :> FSharpSymbol |> (fun f -> f.FullName)
        let typefullname = typ.FullName.Replace("+",".")
        symbolfullname = typefullname
           
    let tryGetALObjectKind (x:t) =
        let alAttributes =
            x.Attributes
            |> Seq.where (fun f -> f.AttributeType.AccessPath = "Fs.AL.Core.Abstract.AL")
            |> Seq.toList
        let v = 1
        let compiledName = x |> getALCompiledName
        
        let rec resolveALType (attrs:FSharpAttribute list) =
            match attrs with
            | [] -> None
            | head :: tail ->
                if head.AttributeType |> FSharpEntity.isOfType typeof<AL.Codeunit>
                then Complex (Codeunit x.DisplayName) |> Some
                elif head.AttributeType |> FSharpEntity.isOfType typeof<AL.Table>
                then Complex (Record x.DisplayName) |> Some
                else resolveALType tail
        
        let rec basetypeloop typ = 
            match typ with
            | None -> None
            | Some basetype ->
                if basetype |> FSharpType.hasBaseType<ALCodeunit>
                then Complex (Codeunit compiledName) |> Some
                elif basetype |> FSharpType.isOfType<ALRecord>
                then Complex (Record compiledName) |> Some
                else basetypeloop basetype.BaseType
        
        match resolveALType alAttributes with
        | Some attrtype -> Some attrtype 
        | None -> basetypeloop x.BaseType
        
        
    let hasAttribute<'t> (x:t) =
        x.HasAttribute<'t>() // returns everything that contains attr
        
    let tryGetAttribute<'t> (attrType:Type) (x:t) =
        x.TryGetAttribute<'t>() // returns everything that contains attr
    
    let getMembersFunctionsAndValues (x:t) =
        x.TryGetMembersFunctionsAndValues()
        
    let getALCompiledName (x:t) =
        match x.BaseType with
        | Some bt when bt |> FSharpType.hasBaseType<ALObjectValue> -> x.CompiledName
        | _ -> 
        match x.TryGetAttribute<ALName>() with
        | Some alname -> alname.ConstructorArguments[0] |> snd :?> string
        | None -> 
            match x.TryGetAttribute<CompiledNameAttribute>() with
            | Some compname -> compname.ConstructorArguments[0] |> snd :?> string
            | None -> x.DisplayName


module FSharpMemberOrFunctionOrValue =
    
    type private t = FSharpMemberOrFunctionOrValue
    let isRefOperator (x:t) = x.FullName = Fullname.Operators.ref
    let isNot (x:t) = x.FullName = Operators.``not``
    
//    let isString (x:t) = x.FullName = FullNameOperators.string
//    let getDisplayName (x:t) =
//        if x.IsCompilerGenerated then x.DisplayName
//        else x.FullType.AbbreviatedType.TypeDefinition.DisplayName
        
        

            
    
    
        
        