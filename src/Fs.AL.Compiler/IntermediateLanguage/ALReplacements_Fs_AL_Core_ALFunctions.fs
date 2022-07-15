[<RequireQualifiedAccess>]
module Fs.AL.Compiler.IntermediateLanguage.ALReplacements_Fs_AL_Core_ALFunctions

open System
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.Fullname
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.ALSimpleValues

let ``Fs.AL.Core.ALFunctions.ALDialog.Message`` = 
    
    { new IALFunctionReplacement with
        member this.FunctionName = "Fs.AL.Core.ALFunctions.ALDialog.Message"
        member this.Replacement =
            (fun ctx ->
                ALExpression.createMemberAccessInvocation
                    (Identifier "Dialog") "Message" (ctx.argExprs |> List.map ctx.toAL)  )
    }
    
    
let ``Fs.AL.Core.ALFunctions.ALDialog.Error`` = 
    
    { new IALFunctionReplacement with
        member this.FunctionName = "Fs.AL.Core.ALFunctions.ALDialog.Error"
        member this.Replacement =
            (fun ctx ->
                ALExpression.createMemberAccessInvocation
                    (Identifier "Dialog") "Message" (ctx.argExprs |> List.map ctx.toAL)  )
    }
    
let ``Fs.AL.Core.ALFunctions.Root.Evaluate`` = 
    
    { new IALFunctionReplacement with
        member this.FunctionName = "Fs.AL.Core.ALFunctions.Evaluate" //static fn
        member this.Replacement =
            (fun ctx ->
                let c = 1
                ALExpression.createInvocation
                    "Evaluate" (ctx.argExprs |> List.map ctx.toAL)  )
    }
    

