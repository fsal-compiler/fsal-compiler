namespace Fs.AL

open System
open System.Collections.Generic
open Fs.AL.Core.ALCoreValues
open Microsoft.Dynamics.Nav.CodeAnalysis.SymbolReference

////////// TYPES
///

module BcTypes =

//    module BcFieldType =
//        type Code20 = string
//        type Text100 = string
//        type TextLong = string
//        type Date = DateTime // todo : time
//        type TODO = string // todo : time
//        
        let (|Prefix|_|) (p: string) (s: string) =
            if s.StartsWith(p) then
                Some(s.Substring(p.Length))
            else
                None

        let fromBcTypeDefinitionName (bcTypeDefinition: string) =
            match bcTypeDefinition with
            | "Code[20]" -> typeof<ALCode>
            | "Text[100]" -> typeof<string>
            | "Boolean" -> typeof<bool>
            | "Integer" -> typeof<int>
            | "Date" -> typeof<DateOnly>
            | "DateTime" -> typeof<DateTime>
            | "Blob" -> typeof<byte []>
            | Prefix "Text[" substr -> typeof<string>
            | _ -> typeof<string>
            
        let inline getBcTypeDefaultInstance (bcTypeDefinition: string) =
            match bcTypeDefinition with
            | "Code[20]" ->  Unchecked.defaultof<string>
            | "Text[100]" -> Unchecked.defaultof<string>
            | "Boolean" -> Unchecked.defaultof<bool> |> unbox 
            | "Integer" -> Unchecked.defaultof<int> |> unbox 
            | "Date" -> Unchecked.defaultof<DateTime> |> unbox 
            | "DateTime" -> Unchecked.defaultof<DateTime> |> unbox 
            | "Blob" ->  null
            | Prefix "Text[" substr -> Unchecked.defaultof<string>
            | _ -> failwith "asd"
            


    type ALObjectType =
        | Table of TableDefinition
        | Page of PageDefinition


//    type BcIdAttribute(id: int) =
//        inherit System.Attribute()
//        let mutable _id = id
//        member this.Id : int = _id


//
//    type BcObject(id:int) =
//        let mutable _id = id
//        member this.Id : int = _id
//        static member getId (t:#BcObject) = t.Id





// Put any utilities here

open BcTypes

[<AutoOpen>]
module internal Utilities =

    let x = 1

// Put any runtime constructs here


type ProviderBase() =
    inherit Dictionary<string,obj> ()
    
    // TODO: cache
    
    // TODO: get rid of codeanalysis dll dependency
    

type DataSource(filename: string) =
    member this.FileName = filename


// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly: CompilerServices.TypeProviderAssembly("Fs.AL.Typeprovider.DesignTime.dll")>]
do ()
