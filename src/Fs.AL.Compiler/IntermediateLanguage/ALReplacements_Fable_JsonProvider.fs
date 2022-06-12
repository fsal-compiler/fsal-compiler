[<RequireQualifiedAccess>]
module Fs.AL.Compiler.IntermediateLanguage.ALReplacements_Fable_JsonProvider

open System
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.Fullname
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.ALSimpleValues

/// "Fable.JsonProvider.getProp"
let ``JsonProvider.getProp`` = 
    
    { new IALFunctionReplacement with
        member this.FunctionName = "Fable.JsonProvider.getProp"
        member this.Replacement =
            (fun rargs ->
                //a.SelectToken('something',outputvar)
                (rargs.toAL rargs.argExprs[0],"SelectToken",[rargs.toAL rargs.argExprs[1]])
                |> InvocationWithoutLastArg
                |> FSALExpr                           
            )
    }
    
    
/// "Fable.JsonProvider.jsonParse"
let ``JsonProvider.jsonParse`` = 
    
    { new IALFunctionReplacement with
        member this.FunctionName = "Fable.JsonProvider.jsonParse"
        member this.Replacement =
            (fun rargs ->
                //a.SelectToken('something',outputvar)
                ("ReadFrom",rargs.argExprs |> List.map rargs.toAL)
                |> InvocationWithoutTarget
                |> FSALExpr                         
            )
    }

