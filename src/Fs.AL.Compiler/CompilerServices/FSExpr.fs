namespace Fs.AL.Compiler.CompilerServices

open FSharp.Compiler.Symbols

[<AutoOpen>]
[<RequireQualifiedAccess>]
module FSExpr =
    
    type private t = FSharpExpr
    
    let groupSequentials (_curr:FSharpExpr) (_next:FSharpExpr) =
        let rec chainedSequentials acc next' =
            match next' with
            | FSharpExprPatterns.Sequential(curr,nextExpr) ->
                chainedSequentials (curr :: acc) nextExpr
            | x -> x :: acc
        let chain = chainedSequentials [_curr] _next
        chain |> List.rev
        
        
    let isConst (e:t) =
        match e with
        | FSharpExprPatterns.Const v -> true
        | _ -> false


    let getPatternName (e:t) =
        match e with
        | FSharpExprPatterns.Const(constValueObj, constType) -> $"Const:{constValueObj}"

        | FSharpExprPatterns.Let((bindingVar, bindingExpr, debug1), bodyExpr) -> $"Let:{bindingVar}"
        | FSharpExprPatterns.WhileLoop v -> $"WhileLoop:{v}"
        | FSharpExprPatterns.Call v -> "Call"
        | FSharpExprPatterns.Lambda(lambdaVar, bodyExpr) -> $"Lambda:{lambdaVar}"
        | FSharpExprPatterns.Value (v) -> $"Value:{v}"
        | FSharpExprPatterns.Sequential (curr,next) -> $"Sequential:{curr.Range.Start}"
        | FSharpExprPatterns.Coerce v -> "Coerce"
        | FSharpExprPatterns.ValueSet(valToSet, valueExpr) -> $"ValueSet:{valToSet}"
        | FSharpExprPatterns.TryFinally(bodyExpr, finalizeExpr, debug1, debug2) -> $"TryFinally:{bodyExpr.Range.Start}" 
        | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) -> $"DecisionTree:{decisionExpr.Range.Start}"
        | FSharpExprPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) -> $"DecisionTreeSuccess:{decisionTargetIdx}"
        | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> $"IfThenElse:{guardExpr.Range.Start}"
        | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) -> $"NewObject:{objType}"
        | FSharpExprPatterns.AddressOf(lvalueExpr) -> $"AddressOf:{lvalueExpr}"
        | FSharpExprPatterns.NewRecord(recordType, argExprs) -> $"NewRecord:%A{recordType}"
        | FSharpExprPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> $"FSharpFieldGet:%A{fieldInfo.Name}"
        | FSharpExprPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp, debug1, debug2) -> $"FastIntegerForLoop:%A{startExpr.Range.Start}"

        | v ->
            let vt = v
            "-----"
        
    let rec getLastSequence (e:t) =
        match e with
        | FSharpExprPatterns.Sequential(curr, next) ->
            getLastSequence next
        | x -> x