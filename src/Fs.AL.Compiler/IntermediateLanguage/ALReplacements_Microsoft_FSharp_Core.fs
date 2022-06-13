[<RequireQualifiedAccess>]
module Fs.AL.Compiler.IntermediateLanguage.ALReplacements_Microsoft_FSharp_Core

open System
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.Fullname
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.ALSimpleValues

/// "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetArray"
let GetArray = 
    
    { new IALFunctionReplacement with
        member this.FunctionName = "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetArray"
        member this.Replacement =
            (fun ctx ->
                match ctx.argExprs[0],ctx.argExprs[1] with
                | FSharpExprPatterns.Value(v), get_idx ->
                    let vtype = v.FullType |> FSharpType.tryGetGenericTypeArg |> Option.map (FSharpType.getRootType)
                    let vtypefullname =  vtype |> Option.map FSharpType.getFullName
                    match vtypefullname with
                    | Some Types.String | Some Types.Double | Some Types.Int32 ->
                        let onebasedIndex = get_idx |> ctx.toAL |> ALExpression.convertTo1BasedIndex 
                        let idf = ALExpression.createIdentifer v
                        ALExpression.createMemberAccessInvocation idf "Get" [onebasedIndex]
                    | Some t ->
                        let genfstype = vtype.Value |> ALType.ofFSharpType
                        let v1 = 1
                        match genfstype with
                        | Simple JsonToken ->
                            let idf = ALExpression.createIdentifer v
                            let zerobasedIndex = get_idx |> ctx.toAL
                            
                            match ALExprContext.getLetBindingCtx ctx.context with
                            | None -> raise (NotImplementedException()) 
                            | Some (assignTo,LetExprKind.TypeProvider _) ->
                                let v = 1
                                let assignToIdentifer = ALExpression.createIdentifer assignTo
                                let statements =
                                    [
                                        ALExpression.createMemberAccessInvocation idf "Get" [zerobasedIndex ; Identifier "_jtoken"]  |> ALStatement.ofExpression
                                        Assignment (assignToIdentifer,(Identifier "_jtoken"))
                                    ]
                                ALStatement.sequenceOf statements |> ALExpression.fromALStatement
                                
                            | _ -> raise (NotImplementedException($"Get Index for type {t} not implemented"))
                        | _ -> raise (NotImplementedException($"Get Index for type {t} not implemented"))
                    | t -> 
                        raise (NotImplementedException($"Get Index for type {t} not implemented"))
                    
                | FSharpExprPatterns.Coerce(targetType, inpExpr), FSharpExprPatterns.Const(idx,_) ->
                    let vtx = 1
                    let (letBindVal,letBindValExpr) = (ALExprContext.getLetBindingCtx ctx.context).Value
                    let t = inpExpr |> ctx.toAL // selectToken 'nicknames'
                    match t with
                    | FSALExpr (InvocationWithoutLastArg (Identifier target,methodname,args)) ->
                        let targetJsonProp : string  = args[0] |> ALExpression.getConstValue |> unbox 
                        let jVarName = $".{target}.{targetJsonProp}Token@{ctx.variableDeclarations.Count}" 
                        let jArrayVarName = $".{target}.{targetJsonProp}Array@{ctx.variableDeclarations.Count}" 
                        // declare intermediary variables
                        [
                            ALVariable.createSimple jVarName JsonToken    
                            ALVariable.createSimple jArrayVarName JsonArray    
                        ]
                        |> List.iter ctx.variableDeclarations.Add
                        

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
    
    


