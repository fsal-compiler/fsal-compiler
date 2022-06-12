module rec Fs.AL.Compiler.ExpressionReader

open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerServices
open Fs.AL.Compiler.IntermediateLanguage.ALContext
open Fs.AL.Compiler.IntermediateLanguage.ALExpressions
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage

module Debugging =
    do ()

let readProcedureBody (b:ALProcedureContext) (body:FSharpExpr) =
    let toFSAL (exp:ALExpression) = exp |> ALStatement.ofExpression 
    Logger.logDebug $"read: %s{FSExpr.getPatternName body}"
    b.statements <-
        ALExpression.ofFSharpExpr b body |> toFSAL 
    b
    
