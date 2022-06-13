[<RequireQualifiedAccess>]
module Fs.AL.Compiler.IntermediateLanguage.ALTypeReplacements

open FSharp.Compiler.Symbols
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALCoreValues

//let primitives =
//    Assembly.GetAssembly(typeof<System.Int32>).GetTypes()
//    |> Seq.where (fun f -> f.IsPrimitive)
//    |> Seq.map (fun f -> $"let [<Literal>] {f.Name} = \"{f.FullName}\"")
//    |> Seq.iter Console.WriteLine

// Some common ones
let SupportedCoreLibTypes =
    [|
        "System.Int32", (Simple Integer)
        "System.String", (Simple (Text None))
        "System.DateTime", (Simple DateTime)
        "System.Object", (Complex Variant)
        "System.Boolean", (Simple Boolean)
        "System.Double", (Simple Decimal)
        "System.Char", (Simple Char)
        "System.Text.Json.Nodes.JsonArray", Simple JsonArray
        "System.Text.Json.Nodes.JsonNode", Simple JsonToken
        "System.Text.Json.JsonSerializer", Simple JsonToken
    |]

let (|HasCoreLibType|_|) (s:string) =  
    SupportedCoreLibTypes
    |> Array.tryFind (fun (name,rep) -> name = s)
    |> Option.map snd


let (|HasSupportedCoreLibType|_|) (s:FSharpType) =  
    let fullname = (s.TypeDefinition:> FSharpSymbol).FullName
    SupportedCoreLibTypes
    |> Array.tryFind (fun (name,rep) -> name = fullname)
    |> Option.map snd


//
//let CustomTypeMappings =
//    [|
//        "Fable.JsonProvider.Generator<...>", (Simple JsonToken)
//    |]
//
//let (|HasCustomType|_|) (s:string) =  
//    CustomTypeMappings
//    |> Array.tryFind (fun (name,rep) -> name = s)
//    |> Option.map snd
//    
//    
//    



