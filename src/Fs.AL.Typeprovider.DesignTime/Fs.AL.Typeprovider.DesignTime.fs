module Fs.AL.TypeproviderImplementation

open System
open System.Reflection
open FSharp.Core.CompilerServices
open Fs.AL.Core.ALComplexValues
open Fs.AL.Typeprovider.DesignTime

open Fs.AL.Typeprovider.DesignTime.ALHelpers
open ICSharpCode.SharpZipLib.Zip
open Microsoft.Dynamics.Nav.CodeAnalysis.SymbolReference
open Fs.AL.Typeprovider
open Fs.AL
open Fs.AL.BcTypes
open ProviderImplementation.ProvidedTypes

open Fs.AL.Typeprovider.CommonHelpers      
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading
open System.IO
open System.Collections.Concurrent
open System.Threading


// Put any utility helpers here
[<AutoOpen>]
module internal Helpers =
    let x = 1

    let createTypeProviderWithCache (asm:Assembly,ns:string) (typeName:string) (afterPathResolvedFn: string -> string -> ProvidedTypeDefinition)=
        let mainType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<ProviderBase>, isErased = false)
        mainType.DefineStaticParameters( 
            [ProvidedStaticParameter("Path", typeof<string>)], 
                fun typeName args -> afterPathResolvedFn typeName (unbox<string> args.[0])
        )
        mainType

[<AutoOpen>]        
type Cache() =        
    static member val counter = 0 with get,set
    static member val monitor = System.Object()
    static member val appPackageCache = Dictionary<string,ALObjectType[]>()
    static member val providerCache = Dictionary<string,ProvidedTypeDefinition>() 
    

[<TypeProvider>]
type BasicGenerativeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Fs.AL.Typeprovider.DesignTime", "Fs.AL.Typeprovider.Runtime")])

    let asm = Assembly.GetExecutingAssembly()
    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name) 
    let ns = "Fs.AL"
     
    let getALTables (asm: Assembly, ns) (symbolCache: ALObjectType []) =
        symbolCache
        |> Seq.choose
            (fun f ->
                match f with
                | Table tbl -> Some tbl
                | _ -> None)
        |> Seq.distinctBy (fun f -> f.Name ) // some weird bug 
        |> Seq.map (TableDefinition.createALTable (asm,ns))
            
    let getTablesType (asm,ns) alObjects =
            let t = ProvidedTypeDefinition(asm,ns,"Tables", Some typeof<obj>, hideObjectMethods = true, isErased = false)
            let addTables() = 
                let time = System.DateTime.Now.Ticks
                let mutable count = 0
                for table in getALTables (asm,ns) alObjects do
                    t.AddMember(table)
                    count <- count+1
                let duration = System.TimeSpan(System.DateTime.Now.Ticks-time).Milliseconds
                Logger.log $"added %i{count} types in %i{duration} ms"
            addTables()
            t            
 
    /// read zip only once
    let getSymbolsPackage (path:string) =
        // use mutex = new System.Threading.Mutex(true, path)
        // mutex.WaitOne(200_000) |> ignore
        let shortpath = path |> System.IO.FileInfo |> (fun f -> f.Name)
        match appPackageCache.TryGetValue(path) with
        | true,pkg -> 
            Logger.log (sprintf "read symbols cache for %s" shortpath)
            pkg
        | false,_ -> 
            let alPackage =
                let starttime = System.DateTime.Now.Ticks
                Logger.log (sprintf "loading symbols cache for %s" shortpath)
                use archive = new ZipFile(path)
//                let assembly = @"C:\Users\kast\source\myrepos\Fs.AL\src\Fs.AL.Compiler\Newtonsoft.Json.dll" |> File.ReadAllBytes
//                AppDomain.CurrentDomain.Load(assembly)
//                System.Reflection.Assembly.LoadFrom()
                let symbols =
                    "SymbolReference.json"
                    |> archive.GetEntry
                    |> archive.GetInputStream
                    |> SymbolReferenceJsonReader.ReadModule
                    |> (fun f -> f.Tables)
                    |> Array.map (fun f -> Table f)
                let duration = System.TimeSpan(System.DateTime.Now.Ticks-starttime).Milliseconds
                Logger.log (sprintf "%i ms: symbols for %s" duration shortpath)
                symbols
            appPackageCache.TryAdd(key=path,value=alPackage) |> ignore
            alPackage
            

        
            
                 
    let asm = ProvidedAssembly()
    
    
                
    let createProviderType (providedTypeName:string) (appPackagePath:string) : ProvidedTypeDefinition =
        // repeating calls hit cache
        let shortpath = appPackagePath.Split([|'\\';'/'|]) |> Array.last
     
        Cache.counter <- counter + 1 
        
        // CACHING
        // match providerCache.TryGetValue(appPackagePath) with
        // | true, mut when mut <> Unchecked.defaultof<ProvidedTypeDefinition> -> 
        //     Logger.log $"cache hit: %A{counter}"
        //     providerCache.getv(appPackagePath)
        // | false,_ -> 

        //let mutex = new System.Threading.Mutex(true, appPackagePath)
        // mutexes.TryAdd(appPackagePath,mutex) |> ignore
        if System.IO.File.Exists(appPackagePath) |> not then failwith "file does not exist" else
        lock monitor (fun () ->
            let lazytype =
                let starttime = System.DateTime.Now.Ticks
                Logger.log (sprintf "createProviderType invoked with %s" shortpath)
                let bcAppPackageType = CommonHelpers.TypeDefinition.createGenerativeType (asm,ns) providedTypeName
                bcAppPackageType |> CommonHelpers.Constructor.addDefaultConstructor
                let alPackage = getSymbolsPackage(appPackagePath)

                
                let getMembers() =
                    try 
                    [
                            yield getTablesType (asm,ns) alPackage
                    ]
                    with e -> 
                        Logger.log (sprintf "ERROR %s %s" e.Message e.StackTrace)
                        failwithf "ERROR %s %s" e.Message e.StackTrace

                bcAppPackageType.AddMembersDelayed(getMembers)
                asm.AddTypes [ bcAppPackageType ]
                let duration = System.TimeSpan(System.DateTime.Now.Ticks-starttime).Milliseconds
                Logger.log (sprintf "%i ms: types for %s" duration shortpath)
                bcAppPackageType
            providerCache.TryAdd(appPackagePath,lazytype) |> ignore
            lazytype
        )

    let main =
        
        //AL Provider Type
        CommonHelpers.TypeDefinition.createMainTypeWithPathParameter
            (asm,ns) "ALProvider" (fun typeName args -> 
                createProviderType typeName (unbox<string> args))
    do
        this.AddNamespace(ns, [main])




[<TypeProviderAssembly>]
do ()
