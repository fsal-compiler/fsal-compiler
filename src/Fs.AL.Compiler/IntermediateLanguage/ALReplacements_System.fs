[<RequireQualifiedAccess>]
module Fs.AL.Compiler.IntermediateLanguage.ALReplacements_System

open System
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.Fullname
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.ALSimpleValues

/// System.IO.TextWriter.WriteLine
let ``TextWriter.WriteLine`` = 
    
    { new IALFunctionReplacement with
        member this.FunctionName = "System.IO.TextWriter.WriteLine"
        member this.Replacement =
            (fun rargs ->
                let objTarget = rargs.objExpr.Value 
                match objTarget with
                | FSharpExprPatterns.Call(_, memberOrFunc, _, _, _) ->
                    let fname = memberOrFunc |> FSharpSymbol.getTypeFullName // val stdout
                    match memberOrFunc.FullName with
                    | Operators.stdout ->
//                        let ctx = rargs.context |> ALExprContext.get
                        ALExpression.createMemberAccessInvocation
                            (Identifier "Dialog")
                            "Message"
                            (rargs.argExprs |> List.map rargs.toAL)
                    | _ -> raise (NotImplementedException())
                | _ -> raise (NotImplementedException())                             
            )
    }
    
    


