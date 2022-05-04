[<RequireQualifiedAccess>]
module Fs.AL.Compiler.IntermediateLanguage.TypeReplacements

open FSharp.Compiler.Symbols
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALCoreValues

//let primitives =
//    Assembly.GetAssembly(typeof<System.Int32>).GetTypes()
//    |> Seq.where (fun f -> f.IsPrimitive)
//    |> Seq.map (fun f -> $"let [<Literal>] {f.Name} = \"{f.FullName}\"")
//    |> Seq.iter Console.WriteLine

// PRIMITIVES
module FullNames =
    let [<Literal>] Byte = "System.Byte"
    let [<Literal>] Char = "System.Char"
    let [<Literal>] Double = "System.Double"
    let [<Literal>] Int16 = "System.Int16"
    let [<Literal>] Int64 = "System.Int64"
    let [<Literal>] IntPtr = "System.IntPtr"
    let [<Literal>] SByte = "System.SByte"
    let [<Literal>] Single = "System.Single"
    let [<Literal>] UInt16 = "System.UInt16"
    let [<Literal>] UInt32 = "System.UInt32"
    let [<Literal>] UInt64 = "System.UInt64"
    let [<Literal>] UIntPtr = "System.UIntPtr"
    let [<Literal>] String = "System.String"
    let [<Literal>] Int32 = "System.Int32"
    let [<Literal>] Boolean = "System.Boolean"
    let [<Literal>] Float32 = "System.Float32"
    let [<Literal>] JsonValue = "System.Text.Json.Nodes.JsonValue"
    let [<Literal>] JsonArray = "System.Text.Json.Nodes.JsonArray"
    let [<Literal>] JsonObject = "System.Text.Json.Nodes.JsonObject"
    let [<Literal>] FsArray = "Microsoft.FSharp.Core.[]"
    

// Some common ones
let CoreLibTypeMappings =
    [|
        "System.Int32", (Simple Integer)
        "System.String", (Simple (Text None))
        "System.DateTime", (Simple DateTime)
        "System.Object", (Complex Variant)
        "System.Boolean", (Simple Boolean)
        "System.Double", (Simple Decimal)
        "System.Char", (Simple Char)
        "System.Text.Json.Nodes.JsonArray", (Simple (SimpleType "JsonArray"))
        "System.Text.Json.Nodes.JsonNode", Simple (SimpleType "JsonToken")
        "System.Text.Json.JsonSerializer", Simple (SimpleType "JsonToken")
    |]

let (|HasCoreLibType|_|) (s:string) =  
    CoreLibTypeMappings
    |> Array.tryFind (fun (name,rep) -> name = s)
    |> Option.map snd
    
   

let replacementFunctions : ( string * ((FSharpExpr -> ALExpression) -> FSharpExpr option -> obj -> FSharpExpr list -> ALExpression))[] =
    [|
        "System.String.get_Length",
        (fun toAL obj mem args -> ALExpression.createInvocation "StrLen" [toAL obj.Value] )
        "System.Text.Json.JsonSerializer.Deserialize",
        (fun toAL obj mem args -> ALExpression.createInvocation "ReadFrom" [toAL obj.Value] )
        "System.Text.Json.Nodes.JsonNode.Parse",
        (fun toAL obj mem args -> ALExpression.createInvocation "ReadFrom" [toAL obj.Value] )
        "System.DateTime.get_Now",
        (fun toAL obj mem args -> ALExpression.createInvocation "CurrentDateTime" [] )
        "Fable.JsonProvider.jsonParse",
        (fun toAL obj mem args ->
            ("ReadFrom",args |> List.map toAL)
            |> InvocationWithoutTarget
            |> FSALExpr 
        )
        "Fable.JsonProvider.getProp",
        (fun toAL obj mem args ->
            // <target>.<member>
            //a.SelectToken('something',outputvar)
            (toAL args[0],"SelectToken",[toAL args[1]])
            |> InvocationWithoutLastArg
            |> FSALExpr
        )
        "Fs.AL.Core.ALFunctions.ALDialog.Message",
        (fun toAL obj mem args -> ALExpression.createMemberAccessInvocation (Identifier "Dialog") "Message" (args |> List.map toAL)  )
        "Fs.AL.Core.ALFunctions.ALDialog.Error",
        (fun toAL obj mem args -> ALExpression.createMemberAccessInvocation (Identifier "Dialog") "Error" (args |> List.map toAL)  )
        "System.Console.WriteLine",
        (fun toAL obj mem args -> ALExpression.createMemberAccessInvocation (Identifier "Dialog") "Message" (args |> List.map toAL)  )
        "System.String.Substring",
        (fun toAL obj mem args ->
            match args[0] with
            // add integer if it's a constant
            | FSharpExprPatterns.Const(v,g) -> ALExpression.createMemberAccessInvocation (toAL obj.Value) "Substring" [Constant (1 + unbox v)]
            | _ -> ALExpression.createMemberAccessInvocation (toAL obj.Value) "Substring" [Binary (Constant 1 ,ALBinaryOperator.Add, toAL args[0])]
        )
    
    |]


let CustomTypeMappings =
    [|
        "Fable.JsonProvider.Generator<...>", (Simple (SimpleType "JsonToken"))
    |]

let (|HasCustomType|_|) (s:string) =  
    CustomTypeMappings
    |> Array.tryFind (fun (name,rep) -> name = s)
    |> Option.map snd
    
    
    
let (|HasReplacementFunction|_|)
    (toAL) (objExpOpt) (memberOrFunc:FSharpMemberOrFunctionOrValue) (argExprs) (s:string) =
    replacementFunctions
    |> Array.tryFind (fun (name,rep) -> name = s)
    |> Option.map (snd)



