[<RequireQualifiedAccess>]
module Fs.AL.Compiler.IntermediateLanguage.ALReplacements_Microsoft_FSharp_Core

open System
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.Fullname
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.ALSimpleValues

let replacements = [
    { new IALFunctionReplacement with
        member this.FunctionName = "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetArray"
        member this.Replacement =
            (fun rargs ->
                match rargs.argExprs[0],rargs.argExprs[1] with
                | FSharpExprPatterns.Value(v), get_idx ->
                    let vtype = v.FullType |> FSharpType.tryGetGenericTypeArg |> Option.map (FSharpType.getRootType >> FSharpType.getFullName)
                    match vtype with
                    | Some Types.String | Some Types.Double | Some Types.Int32 ->
                        let onebasedIndex = get_idx |> rargs.toAL |> ALExpression.convertTo1BasedIndex 
                        let idf = ALExpression.createIdentifer v
                        ALExpression.createMemberAccessInvocation idf "Get" [onebasedIndex]
                    | t -> raise (NotImplementedException($"Get Index for type {t} not implemented"))
                    
                | FSharpExprPatterns.Coerce(targetType, inpExpr), FSharpExprPatterns.Const(idx,_) ->
                    let vtx = 1
                    let (letBindVal,letBindValExpr) = (ALExprContext.getLetBindingCtx rargs.context).Value
                    let t = inpExpr |> rargs.toAL // selectToken 'nicknames'
                    match t with
                    | FSALExpr (InvocationWithoutLastArg (Identifier target,methodname,args)) ->
                        let targetJsonProp : string  = args[0] |> ALExpression.getConstValue |> unbox 
                        let jVarName = $".{target}.{targetJsonProp}Token@{rargs.variableDeclarations.Count}" 
                        let jArrayVarName = $".{target}.{targetJsonProp}Array@{rargs.variableDeclarations.Count}" 
                        // declare intermediary variables
                        [
                            ALVariable.createSimple jVarName JsonToken    
                            ALVariable.createSimple jArrayVarName JsonArray    
                        ]
                        |> List.iter rargs.variableDeclarations.Add
                        

                        let allExpressions =
                            [
                                ALStatement.createSelectToken "_jtoken" (args @ [Identifier jVarName])
                                ALStatement.createAssignment jArrayVarName
                                    (ALExpression.createJsonCastExpr jVarName targetType)
                                ALStatement.createMemberCall jArrayVarName "Get" ([Constant idx] @ [Identifier "_jtoken"])
                                ALStatement.createValAssignment letBindVal
                                    (ALExpression.createJsonCastExpr "_jtoken" letBindVal.FullType)
                                    
                            ]
                            |> ALStatement.sequenceOf 
                          
                        allExpressions |> ALExpression.fromALStatement
                        
                    | _ -> failwithf $"unimplemented %A{t}"
                    
                | _ -> failwithf $"not an array type"
            )
    }
]


