﻿[<Microsoft.FSharp.Core.AutoOpen>]
module Fs.AL.Compiler.IntermediateLanguage.ALTypeMapping

open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.CompilerSymbols.FSharpType
open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract
open Fs.AL.Compiler.Reflection

module ALType =
    
    type private t = ALType
    
    
    let a : string = ""
    let rec ofFSharpType (ftype:FSharpType) =
        let fullname =
            ftype |> FSharpType.getFullName
        match fullname with
        | FullNameFSharp.ref -> ftype.GenericArguments[0] |> ofFSharpType
        | "Microsoft.FSharp.Core.byref" -> ftype.GenericArguments[0] |> ofFSharpType
        | FullNameFSharp.unit -> Simple( ALSimpleType.Text (Some 123))
        // array types
        | "Microsoft.FSharp.Core.[]" ->
            let arrayof = ftype.GenericArguments[0] |> ofFSharpType
            match arrayof with
            | Simple (SimpleType "JsonToken") -> Simple (SimpleType "JsonArray")
            | _ -> failwith "unimplemented"
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
        | x when ftype.TypeDefinition |> FSharpEntity.hasAttribute<ALJson> -> Simple (SimpleType "JsonToken") // use as json type
        // System.** namespace types
        | TypeReplacements.HasCoreLibType mapping -> mapping
        // jsonprovider types
        | x when x.StartsWith "Fable.JsonProvider.Generator<...>" -> Simple (SimpleType "JsonToken")
        // edge cases, lambdas, typeprovider types
        | x when x.StartsWith("+++") ->
            if x.StartsWith("+++JsonProvider:") then Simple (SimpleType "JsonToken")
            elif x.StartsWith("+++Lambda") then
                match ftype.GenericArguments |> Seq.toList with
                | [ a1; a2 ] -> // a1 unit, a2 return type
                    //let td = a2.TypeDefinition |> FSharpEntity.getALObjectKind
                    failwithf $"%A{a2}"
                | _ -> 
                    let debug = 5
                    failwithf $"%A{debug}"
                    Simple (SimpleType "FUNCTION") //TODO:
            else failwith $"unhandled typeprovider {x}"
        | x when ftype.BaseType.Value.TypeDefinition.FullName = ALTypeFullNames.record ->
            let roottype = ftype.TypeDefinition |> FSharpEntity.getRootType
            let name = ftype.TypeDefinition.DisplayName
            Complex (Record (roottype |> FSharpEntity.getALCompiledName))
        // just for debugging
        | x -> failwith "unknown type"
        | x ->
            let d = 5
            ALSimpleType.Text None |> Simple
            
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
//                match name |> Seq.exists (fun f -> quotechars |> Array.contains f )
//                with
//                | true ->  $"Record \"{name}\""
//                | false -> $"Record {name}"
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
            | SimpleType typename ->
                match typename with
                | "JsonToken" -> "JsonToken"
                | "JsonArray" -> "JsonArray"
                | "JsonObject" -> "JsonObject"
                | "JsonValue" -> "JsonValue"
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
        | x -> 
            let d = 5
            failwithf $"%A{x}"

    let oa : int64 = 1