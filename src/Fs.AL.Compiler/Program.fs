module Fs.Al.Compiler.Program

open System
open System.IO
open System.Text.Json
open Fs.AL.Compiler.ALBuilder
open Fs.AL.Compiler.ALCodeGeneration.ALGenerator
open Fs.AL.Compiler.CompilerDeclarations
open Fs.AL.Compiler.CompilerService
open Fs.AL.Compiler.CompilerServices
open Fs.AL.Compiler.IntermediateLanguage
open Microsoft.FSharp.Core

#if DEBUG
//Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__ + @"./../Fs.AL.SampleProject/")
//Directory.SetCurrentDirectory(@"C:\Users\kast\source\publicrepos\fsal-maaamet")
#endif

let settings =
    match "fsal.jsonc" |> File.Exists with
    | false -> failwith "no fsal.jsonc found"
    | true ->
        "fsal.jsonc"
        |> File.ReadAllLines
        |> Array.filter (fun f -> not (f.TrimStart().StartsWith("//")))
        |> String.concat ""
        |> JsonSerializer.Deserialize<FSharpToALCompilerSettings>

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

let replacementfunctions =
    let t = typeof<IALFunctionReplacement>
    let replacements =
        AppDomain.CurrentDomain.GetAssemblies()
        |> Seq.collect (fun f -> f.GetTypes())
        |> Seq.where (fun f ->
            t.IsAssignableFrom(f)
            && not f.IsInterface
        )
        
    replacements
    |> Seq.map (fun f ->
        let inst = Activator.CreateInstance(f) :?> IALFunctionReplacement
        inst.FunctionName,inst
    )
    |> dict
    
let v1 = replacementfunctions
let v2 = 1
     
        
let compilerCtx =
    let builders = fsharpImplementations |> Seq.where (fun impl -> not impl.entity.IsAbstractClass)     
    let abstractClasses = fsharpImplementations |> Seq.where (fun impl -> impl.entity.IsAbstractClass) |> Seq.map ALObjectBuilder.ofEntityWithMembers |> Seq.toArray
    let v = 1
    {
        builders =
            builders
            |> Seq.map ALObjectBuilder.ofEntityWithMembers
            |> Seq.map (fun f -> { f with registeredFunctionReplacements = replacementfunctions })
            |> Seq.toArray
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
