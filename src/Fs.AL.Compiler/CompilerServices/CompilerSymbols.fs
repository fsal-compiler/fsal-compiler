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
    

module FSharpType =
    
    type private t = FSharpType
    
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
    let getFullName (x:FSharpType) =
        x.TypeDefinition |> FSharpEntity.getRootType
        :> FSharpSymbol
        |> (fun f -> f.FullName)

module FSharpEntity =
    
    type private t = FSharpEntity
    
    /// goes through abbreviations
    let rec  getRootType (x:t) =
        let rt = 5
//        if x.Assembly.QualifiedName = "" then "+++JsonProvider:"+x.DisplayName else
        //TODO : test
        
        match x.IsFSharpAbbreviation with
        | false -> x
        | true -> x.AbbreviatedType.TypeDefinition |> getRootType
    
    let rec private getRootTypeFullName (x:t) =
        let rt = 5
//        if x.Assembly.QualifiedName = "" then "+++JsonProvider:"+x.DisplayName else
        //TODO : there might be other provided types
        if x.IsProvided then "System.Text.Json.Nodes.JsonValue" else
        if x.IsArrayType then "System.Text.Json.Nodes.JsonArray" else
        
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
        
    let tryGetType (x:t) =
        let ctor =
            x.MembersFunctionsAndValues
            |> Seq.tryFind (fun f -> f.IsConstructor )
            |> Option.bind (fun f -> f.FullTypeSafe )
            |> Option.map (fun f -> f.GenericArguments[1] )
        ctor
//    let isALType (x:t) =
//        match x.BaseType with
//        | None -> false
//        | Some basetype ->
//            basetype |> FSharpType.isOfType<ALComplexValue>
//            || basetype |> FSharpType.isOfType<ALObject>
//            || basetype |> FSharpType.isOfType<ALCodeunit>
//            
    let hasALType (x:FSharpType option) =
        match x with
        | None -> false
        | Some basetype ->
            basetype |> FSharpType.isOfType<ALComplexValue>
            || basetype |> FSharpType.isOfType<ALObject>
            || basetype |> FSharpType.isOfType<ALCodeunit>
            || basetype |> FSharpType.isOfType<ALCodeunit>
       
    let isALObject (x:t) =
        match x.BaseType with
        | None -> false
        | Some basetype ->
            basetype |> FSharpType.isOfType<ALCodeunit>
            || basetype |> FSharpType.isOfType<ALRecord>
            
            
    let getALObjectKind (x:t) =
        match x.TryGetAttribute<ALSingleInstanceCodeunit>() with
        | Some attr -> Complex (Codeunit x.DisplayName)
        | _ -> 
        let name = x |> FSharpEntity.getALCompiledName
        let rec loop typ = 
            match typ with
            | None -> failwithf $"%A{x} is not an AL Object"
            | Some basetype ->
                if basetype |> FSharpType.hasBaseType<ALCodeunit>
                then Complex (Codeunit name)
                elif basetype |> FSharpType.isOfType<ALRecord>
                then Complex (Record name)
                else loop basetype.BaseType
        loop x.BaseType
    let hasAttribute<'t> (x:t) =
        x.HasAttribute<'t>() // returns everything that contains attr
        
    let tryGetAttribute<'t> (attrType:Type) (x:t) =
        x.TryGetAttribute<'t>() // returns everything that contains attr
    
    let getMembersFunctionsAndValues (x:t) =
        x.TryGetMembersFunctionsAndValues()
        
    let getALCompiledName (x:t) =
        match x.TryGetAttribute<ALSingleInstanceCodeunit>() with
        | Some attr -> x.DisplayName
        | _ ->
        match x.BaseType with
        | Some bt when bt |> FSharpType.hasBaseType<ALObjectValue> -> x.CompiledName
        | _ -> 
        match x.TryGetAttribute<ALName>() with
        | Some alname -> alname.ConstructorArguments[0] |> snd :?> string
        | None -> 
            match x.TryGetAttribute<CompiledNameAttribute>() with
            | None -> failwithf $"has no compiled name" 
            | Some compname -> compname.ConstructorArguments[0] |> snd :?> string
           
        
module FSharpMemberOrFunctionOrValue =
    
    type private t = FSharpMemberOrFunctionOrValue
    let isRefOperator (x:t) = x.FullName = Fullname.Operators.ref
    let isNot (x:t) = x.FullName = Operators.``not``
    
//    let isString (x:t) = x.FullName = FullNameOperators.string
    let getDisplayName (x:t) =
        if x.IsCompilerGenerated then x.DisplayName
        else x.FullType.AbbreviatedType.TypeDefinition.DisplayName
        
        

            
    
    
        
        