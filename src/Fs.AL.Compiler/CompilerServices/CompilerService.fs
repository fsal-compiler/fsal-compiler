module rec Fs.AL.Compiler.CompilerService

open System
open System.IO
open System.Xml.Linq
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerServices



let getReferences (settings:FSharpToALCompilerSettings) =
    let envar x = System.Environment.ExpandEnvironmentVariables x
    [
      yield! (settings.includePaths.Value |> List.map (fun f ->  $"-I:{envar f}"))
      yield! (settings.includeDlls.Value |> List.map (fun f -> $"-r:{envar f}"))
      // sample project dependencies
      // TODO : get dependencies from fsproj
      @"-r:Fs.AL.Packages.BaseApplication.dll"
      @"-r:Fs.AL.Packages.System.dll"
      @"-r:Fs.AL.Core"
      // AL PACKAGE TYPEPROVIDER currently not supported because of .netstandard 2.0 issues 
      // fs core 
//      @"-r:C:\Users\kast\.nuget\packages\fsharp.core\6.0.1\lib\netstandard2.1\FSharp.Core.dll"
      //
      // CORE LIBRARIES
      
      @"-r:mscorlib"
      @"-r:netstandard"
      @"-r:System.AppContext"
      @"-r:System.Buffers"
      @"-r:System.Collections.Concurrent"
      @"-r:System.Collections"
      @"-r:System.Collections.Immutable"
      @"-r:System.Collections.NonGeneric"
      @"-r:System.Collections.Specialized"
      @"-r:System.ComponentModel.Annotations"
      @"-r:System.ComponentModel.DataAnnotations"
      @"-r:System.ComponentModel"
      @"-r:System.ComponentModel.EventBasedAsync"
      @"-r:System.ComponentModel.Primitives"
      @"-r:System.ComponentModel.TypeConverter"
      @"-r:System.Configuration"
      @"-r:System.Console"
      @"-r:System.Core"
      @"-r:System.Data.Common"
      @"-r:System.Data.DataSetExtensions"
      @"-r:System.Data"
      @"-r:System.Diagnostics.Contracts"
      @"-r:System.Diagnostics.Debug"
      @"-r:System.Diagnostics.DiagnosticSource"
      @"-r:System.Diagnostics.FileVersionInfo"
      @"-r:System.Diagnostics.Process"
      @"-r:System.Diagnostics.StackTrace"
      @"-r:System.Diagnostics.TextWriterTraceListener"
      @"-r:System.Diagnostics.Tools"
      @"-r:System.Diagnostics.TraceSource"
      @"-r:System.Diagnostics.Tracing"
      @"-r:System"
      @"-r:System.Drawing"
      @"-r:System.Drawing.Primitives"
      @"-r:System.Dynamic.Runtime"
      @"-r:System.Formats.Asn1"
      @"-r:System.Globalization.Calendars"
      @"-r:System.Globalization"
      @"-r:System.Globalization.Extensions"
      @"-r:System.IO.Compression.Brotli"
      @"-r:System.IO.Compression"
      @"-r:System.IO.Compression.FileSystem"
      @"-r:System.IO.Compression.ZipFile"
      @"-r:System.IO"
      @"-r:System.IO.FileSystem.AccessControl"
      @"-r:System.IO.FileSystem"
      @"-r:System.IO.FileSystem.DriveInfo"
      @"-r:System.IO.FileSystem.Primitives"
      @"-r:System.IO.FileSystem.Watcher"
      @"-r:System.IO.IsolatedStorage"
      @"-r:System.IO.MemoryMappedFiles"
      @"-r:System.IO.Pipes.AccessControl"
      @"-r:System.IO.Pipes"
      @"-r:System.IO.UnmanagedMemoryStream"
      @"-r:System.Linq"
      @"-r:System.Linq.Expressions"
      @"-r:System.Linq.Parallel"
      @"-r:System.Linq.Queryable"
      @"-r:System.Memory"
      @"-r:System.Net"
      @"-r:System.Net.Http"
      @"-r:System.Net.Http.Json"
      @"-r:System.Net.HttpListener"
      @"-r:System.Net.Mail"
      @"-r:System.Net.NameResolution"
      @"-r:System.Net.NetworkInformation"
      @"-r:System.Net.Ping"
      @"-r:System.Net.Primitives"
      @"-r:System.Net.Requests"
      @"-r:System.Net.Security"
      @"-r:System.Net.ServicePoint"
      @"-r:System.Net.Sockets"
      @"-r:System.Net.WebClient"
      @"-r:System.Net.WebHeaderCollection"
      @"-r:System.Net.WebProxy"
      @"-r:System.Net.WebSockets.Client"
      @"-r:System.Net.WebSockets"
      @"-r:System.Numerics"
      @"-r:System.Numerics.Vectors"
      @"-r:System.ObjectModel"
      @"-r:System.Reflection.DispatchProxy"
      @"-r:System.Reflection"
      @"-r:System.Reflection.Emit"
      @"-r:System.Reflection.Emit.ILGeneration"
      @"-r:System.Reflection.Emit.Lightweight"
      @"-r:System.Reflection.Extensions"
      @"-r:System.Reflection.Metadata"
      @"-r:System.Reflection.Primitives"
      @"-r:System.Reflection.TypeExtensions"
      @"-r:System.Resources.Reader"
      @"-r:System.Resources.ResourceManager"
      @"-r:System.Resources.Writer"
      @"-r:System.Runtime"
      @"-r:System.Runtime.Extensions"
      @"-r:System.Runtime.Handles"
      @"-r:System.Runtime.InteropServices"
      @"-r:System.Runtime.Intrinsics"
      @"-r:System.Runtime.Loader"
      @"-r:System.Runtime.Numerics"
      @"-r:System.ServiceModel.Web"
      @"-r:System.ServiceProcess"
      @"-r:System.Text.Encoding.CodePages"
      @"-r:System.Text.Encoding"
      @"-r:System.Text.Encoding.Extensions"
      @"-r:System.Text.Encodings.Web"
      @"-r:System.Text.Json"
      @"-r:System.Text.RegularExpressions"
      @"-r:System.Threading.Channels"
      @"-r:System.Threading"
      @"-r:System.Threading.Overlapped"
      @"-r:System.Threading.Tasks.Dataflow"
      @"-r:System.Threading.Tasks"
      @"-r:System.Threading.Tasks.Extensions"
      @"-r:System.Threading.Tasks.Parallel"
      @"-r:System.Threading.Thread"
      @"-r:System.Threading.ThreadPool"
      @"-r:System.Threading.Timer"
      @"-r:System.Transactions"
      @"-r:System.Transactions.Local"
      @"-r:System.ValueTuple"
      @"-r:System.Web"
      @"-r:System.Web.HttpUtility"
      @"-r:System.Windows"
      @"-r:System.Xml"
      @"-r:System.Xml.Linq"
      @"-r:System.Xml.ReaderWriter"
      @"-r:System.Xml.Serialization"
      @"-r:System.Xml.XDocument"
      @"-r:System.Xml.XmlDocument"
      @"-r:System.Xml.XmlSerializer"
      @"--targetprofile:netcore"
      @"--simpleresolution"
      @"--deterministic+" ]


type Project(fsprojFile: string) =

    /// defaults to fsproj name
    member val Name = fsprojFile |> Path.GetFileNameWithoutExtension with get, set
    
    member val FsProjDirectory = FileInfo(fsprojFile).DirectoryName 
    
    

    member val Checker =
        FSharpChecker.Create(
            keepAssemblyContents = true,
            keepAllBackgroundResolutions = true,
            keepAllBackgroundSymbolUses = true
        ) with get, set

    member this.ProjectFiles =
        lazy (
            let projectDir = this.FsProjDirectory

            let compiledFiles =
                fsprojFile
                |> File.ReadAllText
                |> XElement.Parse
                |> (fun f -> f.Descendants("Compile"))
                |> Seq.choose (fun f ->
                    match f.Attribute("Include") with
                    | null -> None
                    | incl -> incl.Value |> Some)
                |> Seq.map (fun f -> Path.Combine(projectDir, f))
                |> Seq.toArray
            compiledFiles
        )

    /// checks entire project
    member this.GetCheckResults(projOpts) =
        let results =
            this.Checker.ParseAndCheckProject(projOpts)
            |> Async.RunSynchronously

        let failed =
            results.Diagnostics
            |> Seq.exists (fun f ->
                f.Severity = FSharpDiagnosticSeverity.Error)

        match failed with
        | false -> results
        | true -> failwithf $"%A{results.Diagnostics}"



type FSharpProjectOptions with
    static member ofProject (settings:FSharpToALCompilerSettings) (proj: Project) =
        { ProjectFileName = proj.Name
          ProjectId = None
          SourceFiles =
              match settings.includeFiles with
              | None -> proj.ProjectFiles.Value
              | Some f -> f |> Seq.map (fun f -> Path.Combine(proj.FsProjDirectory, f)) |> Seq.toArray
          
          OtherOptions =
            [|
               yield "--define:NET6_0"
               yield "--deterministic+"
               yield! (getReferences settings)
               |]
          ReferencedProjects = [| |]
          IsIncompleteTypeCheckEnvironment = false
          UseScriptResolutionRules = false
          LoadTime = DateTime.Now
          UnresolvedReferences = None
          OriginalLoadReferences = []
          Stamp = None }


let getDefaultProjectDeclarations settings (fsprojPath: string) =

    //    let fsproj = @"C:\Users\kast\source\myrepos\Fs.AL\src\Fs.AL.SampleProject\Fs.AL.SampleProject.fsproj"
    let project = Project(fsprojPath)
    //let projfiles = project.ProjectFiles.Value
    let projOpts = FSharpProjectOptions.ofProject settings project 
    let checkResults = project.GetCheckResults(projOpts)
    
    let implementations = checkResults.AssemblyContents.ImplementationFiles
    
    // TODO: certain parts still require implementation for optimized assembly
//    let implementations = checkResults.GetOptimizedAssemblyContents().ImplementationFiles
        
    implementations
    |> Seq.collect (fun f -> f.Declarations)
