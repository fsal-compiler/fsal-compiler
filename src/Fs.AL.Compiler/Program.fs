module Fs.Al.Compiler.Program

open System
open System.IO
open System.Text.Json
open Fs.AL.Compiler.ALBuilder
open Fs.AL.Compiler.ALCodeGeneration.ALGenerator
open Fs.AL.Compiler.CompilerDeclarations
open Fs.AL.Compiler.CompilerService
open Microsoft.FSharp.Core

let settings =
    match "fsal.json" |> File.Exists with
    | false -> failwith "no fsal.json found"
    | true ->
        "fsal.json"
        |> File.ReadAllText
        |> JsonSerializer.Deserialize<FSharpToALCompilerSettings>

//let testsettings =
//    {
//        sourcePath = Some @"C:\Users\kast\source\myrepos\Fs.AL\src\Fs.AL.SampleProject\fsharp"
//        outputPath = Some @"C:\Users\kast\source\myrepos\Fs.AL\src\Fs.AL.SampleProject\al"
//        includePaths = Some [
//            "C:\\Users\\kast\\source\\myrepos\\Fs.AL\\src\\Fs.AL.SampleProject\\fsharp\\bin\\Debug\\net6.0-windows\\"
//            "C:\\Users\\kast\\source\\myrepos\\Fs.AL\\src\\Fs.AL.SampleProject\\fsharp\\bin\\Debug\\net6.0-windows\\lib\\"
//            "C:\\Program Files\\dotnet\\packs\\Microsoft.NETCore.App.Ref\\6.0.3\\ref\\net6.0"
//        ]
//        includeDlls = Some [
//            "C:\\Users\\kast\\.nuget\\packages\\fable.jsonprovider\\1.0.1\\lib\\netstandard2.0\\Fable.JsonProvider"
//        ]
//    }
//let settings = testsettings

let starttime = DateTime.Now

let fsproj =
    let sourcedir = 
        match settings.sourcePath with
        | None -> Path.Combine(Directory.GetCurrentDirectory(),"fsharp")
        | Some dir -> Path.Combine(Directory.GetCurrentDirectory(),dir)
    Directory.GetFiles(sourcedir,"*.fsproj")
    |> Seq.tryHead
    |> Option.defaultWith (fun f -> failwith $".fsproj file not found in {sourcedir}")
let declarations = getDefaultProjectDeclarations settings fsproj
let fsharpImplementations =
    declarations
    |> Seq.map FSharpImplementation.ofImplementationFileDeclaration
    |> Seq.collect id
    |> Seq.toArray

let compilerCtx =
    let builders = fsharpImplementations |> Seq.where (fun (f,mem) -> not f.IsAbstractClass)     
    let abstractClasses = fsharpImplementations |> Seq.where (fun (f,mem) -> f.IsAbstractClass)  |> Seq.map ALObjectBuilder.ofEntityWithMembers |> Seq.toArray
    {
        builders = builders |> Seq.map ALObjectBuilder.ofEntityWithMembers |> Seq.toArray
        cache = {
            abstractClasses = abstractClasses
            outputPath = settings.outputPath |> Option.defaultValue "al"
        }
    }
  
let runCompiler (ctx:CompilerContext) =
    ctx.builders
    |> Seq.map (ALObjectBuilder.withCache ctx.cache) 
    |> Seq.map ALObjectBuilder.withDefaultObjectId // add object ids
    |> Seq.map ALObjectBuilder.withInheritedMembers // add inherited members
    |> Seq.map ALObjectBuilder.withALFields // add page/table fields
    |> Seq.map ALObjectBuilder.withALMembers // add members
    |> Seq.iter ALObjectBuilder.createALFile // generate AL code
    
runCompiler compilerCtx
    
let duration = TimeSpan(DateTime.Now.Ticks - starttime.Ticks);

printfn $"compilation took: %i{int duration.TotalMilliseconds}ms"
