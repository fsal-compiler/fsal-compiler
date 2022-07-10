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
/// stdout.WriteLine
let ``TextWriter.WriteLine`` = 
    
    { new IALFunctionReplacement with
        member this.FunctionName = "System.IO.TextWriter.WriteLine"
        member this.Replacement =
            (fun rargs ->
                let objTarget = rargs.objExpr.Value 
                match objTarget with
                | FSharpExprPatterns.Call(_, memberOrFunc, _, _, _) ->
                    let fullname = memberOrFunc |> FSharpSymbol.getTypeFullName // val stdout
                    match fullname with
                    | Operators.stdout ->
                        ALExpression.createMemberAccessInvocation
                            (Identifier "Dialog") "Message" (rargs.argExprs |> List.map rargs.toAL)
                    | _ -> raise (NotImplementedException())
                | _ -> raise (NotImplementedException())                             
            )
    }
    
    

//System.Array.get_Length
let ``System.Array.get_Length`` = 
    
    { new IALFunctionReplacement with
        member this.FunctionName = "System.Array.get_Length"
        member this.Replacement =
            (fun rargs ->
                let objTarget,objType =
                    match rargs.objExpr with
                    | Some (FSharpExprPatterns.Value(valueToGet)) ->
                        ALExpression.createIdentifer valueToGet,
                        valueToGet.FullType |> ALType.ofFSharpType
                    | _ -> raise (NotImplementedException())
                    
                match objType with
                | Simple JsonArray -> ALExpression.createMemberAccessInvocation objTarget "Count" []
                | _ -> raise (NotImplementedException())
            )
    }
    
    