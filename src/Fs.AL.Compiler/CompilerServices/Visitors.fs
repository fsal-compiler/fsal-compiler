module Fs.AL.Compiler.Visitors

open FSharp.Compiler.Symbols
open Fs.AL.Compiler

[<RequireQualifiedAccess>]
type ExpressionContext =
    | None
    | Sequential of chain:FSharpExpr list

/// does not go further into recursive member calls
let rec visitFunctionBody (g:ExpressionContext -> unit) (f) (e:FSharpExpr)  =
    f e
    let visitFunctionBody = visitFunctionBody g
    let visitOuterExprs = visitOuterExprs g
    let visitObjArg = visitObjArg g
    let visitObjMember = visitObjMember g
    match e with 
    | FSharpExprPatterns.AddressOf(lvalueExpr) -> 
        visitFunctionBody f lvalueExpr
    | FSharpExprPatterns.AddressSet(lvalueExpr, rvalueExpr) -> 
        visitFunctionBody f lvalueExpr; visitFunctionBody f rvalueExpr
    | FSharpExprPatterns.Application(funcExpr, typeArgs, argExprs) -> 
        visitFunctionBody f funcExpr; visitOuterExprs f argExprs
    | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) -> 
//        visitObjArg f objExprOpt //visitOuterExprs f argExprs
        ()
    | FSharpExprPatterns.Coerce(targetType, inpExpr) -> 
        visitFunctionBody f inpExpr
    | FSharpExprPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp, debug1, debug2) -> 
        visitFunctionBody f startExpr; visitFunctionBody f limitExpr; visitFunctionBody f consumeExpr
    | FSharpExprPatterns.ILAsm(asmCode, typeArgs, argExprs) -> 
        visitOuterExprs f argExprs
    | FSharpExprPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> 
        visitObjArg f objExprOpt
    | FSharpExprPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> 
        visitObjArg f objExprOpt
    | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        //
        ()
//        visitALExpr f guardExpr; visitALExpr f thenExpr; visitALExpr f elseExpr
    
    | FSharpExprPatterns.Lambda(lambdaVar, bodyExpr) -> 
        visitFunctionBody f bodyExpr
    | FSharpExprPatterns.Let((bindingVar, bindingExpr, debug1), bodyExpr) -> 
//        visitALExpr f bindingExpr
        visitFunctionBody f bodyExpr
    | FSharpExprPatterns.LetRec(recursiveBindings, bodyExpr) ->
        List.iter (fun (z,x,c) -> visitFunctionBody f x) recursiveBindings; visitFunctionBody f bodyExpr
    | FSharpExprPatterns.NewArray(arrayType, argExprs) -> 
        visitOuterExprs f argExprs
    | FSharpExprPatterns.NewDelegate(delegateType, delegateBodyExpr) -> 
        visitFunctionBody f delegateBodyExpr
    | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) -> 
        visitOuterExprs f argExprs
    | FSharpExprPatterns.NewRecord(recordType, argExprs) ->  
        visitOuterExprs f argExprs
    | FSharpExprPatterns.NewAnonRecord(recordType, argExprs) ->  
        visitOuterExprs f argExprs
    | FSharpExprPatterns.NewTuple(tupleType, argExprs) -> 
        visitOuterExprs f argExprs
    | FSharpExprPatterns.NewUnionCase(unionType, unionCase, argExprs) -> 
        visitOuterExprs f argExprs
    | FSharpExprPatterns.Quote(quotedExpr) -> 
        visitFunctionBody f quotedExpr
    | FSharpExprPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> 
        visitObjArg f objExprOpt
    | FSharpExprPatterns.AnonRecordGet(objExpr, recordOrClassType, fieldInfo) -> 
        visitFunctionBody f objExpr
    | FSharpExprPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) -> 
        visitObjArg f objExprOpt; visitFunctionBody f argExpr
    | FSharpExprPatterns.Sequential(firstExpr, secondExpr) ->
//        let start = [firstExpr]
//        let rec chainedSequentials acc secondary =
//            match secondary with
//            | FSharpExprPatterns.Sequential(curr,nextExpr) ->
//                let newacc = (curr) :: acc
//                chainedSequentials newacc nextExpr
//            | _ -> acc
//        let chain = chainedSequentials start secondExpr
//        let exprChain = firstExpr :: secondExpr :: secondExpr.ImmediateSubExpressions
//        g (ExpressionContext.Sequential exprChain)
//        let test = 5
        visitFunctionBody f firstExpr
        visitFunctionBody f secondExpr
    | FSharpExprPatterns.TryFinally(bodyExpr, finalizeExpr, debug1, debug2) -> 
        visitFunctionBody f bodyExpr; visitFunctionBody f finalizeExpr
    | FSharpExprPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr, debug1, debug2) -> 
        visitFunctionBody f bodyExpr; visitFunctionBody f catchExpr
    | FSharpExprPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) -> 
        visitFunctionBody f tupleExpr
    | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) -> 
        visitFunctionBody f decisionExpr; List.iter (snd >> visitFunctionBody f) decisionTargets
    | FSharpExprPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) -> 
        visitOuterExprs f decisionTargetExprs
    | FSharpExprPatterns.TypeLambda(genericParam, bodyExpr) -> 
        visitFunctionBody f bodyExpr
    | FSharpExprPatterns.TypeTest(ty, inpExpr) -> 
        visitFunctionBody f inpExpr
    | FSharpExprPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) -> 
        visitFunctionBody f unionExpr; visitFunctionBody f valueExpr
    | FSharpExprPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) -> 
        visitFunctionBody f unionExpr
    | FSharpExprPatterns.UnionCaseTest(unionExpr, unionType, unionCase) -> 
        visitFunctionBody f unionExpr
    | FSharpExprPatterns.UnionCaseTag(unionExpr, unionType) -> 
        visitFunctionBody f unionExpr
    | FSharpExprPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) -> 
        visitFunctionBody f baseCallExpr
        List.iter (visitObjMember f) overrides
        List.iter (snd >> List.iter (visitObjMember f)) interfaceImplementations
    | FSharpExprPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) -> 
        visitOuterExprs  f argExprs
    | FSharpExprPatterns.ValueSet(valToSet, valueExpr) -> 
//        visitFunctionBody f valueExpr
        ()
    | FSharpExprPatterns.WhileLoop(guardExpr, bodyExpr, debug1) -> 
//        visitFunctionBody f guardExpr; visitFunctionBody f bodyExpr
        ()
    | FSharpExprPatterns.BaseValue baseType -> ()
    | FSharpExprPatterns.DefaultValue defaultType -> ()
    | FSharpExprPatterns.ThisValue thisType -> ()
    | FSharpExprPatterns.Const(constValueObj, constType) -> ()
    | FSharpExprPatterns.Value(valueToGet) -> ()
    | _ -> failwith (sprintf "unrecognized %+A" e)

and visitOuterExprs g f exprs = 
    List.iter (visitFunctionBody g f) exprs
and visitObjArg g f objOpt = 
    Option.iter (visitFunctionBody g f) objOpt
and visitObjMember g f memb = 
    visitFunctionBody g f memb.Body
    
    

/// basic visitor
let rec visitExpr (g:ExpressionContext -> unit) f (e:FSharpExpr) = 
    f e
    let visitExpr = visitExpr g
    let visitObjArg = visitObjArg g
    let visitObjMember = visitObjMember g
    let visitExprs fn exprs = List.iter (visitExpr fn) exprs
    match e with 
    | FSharpExprPatterns.AddressOf(lvalueExpr) -> 
        visitExpr f lvalueExpr
    | FSharpExprPatterns.AddressSet(lvalueExpr, rvalueExpr) -> 
        visitExpr f lvalueExpr; visitExpr f rvalueExpr
    | FSharpExprPatterns.Application(funcExpr, typeArgs, argExprs) -> 
        visitExpr f funcExpr; visitExprs f argExprs
    | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) -> 
        visitObjArg f objExprOpt; visitExprs f argExprs
    | FSharpExprPatterns.Coerce(targetType, inpExpr) -> 
        visitExpr f inpExpr
    | FSharpExprPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp, debug1, debug2) -> 
        visitExpr f startExpr; visitExpr f limitExpr; visitExpr f consumeExpr
    | FSharpExprPatterns.ILAsm(asmCode, typeArgs, argExprs) -> 
        visitExprs f argExprs
    | FSharpExprPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> 
        visitObjArg f objExprOpt
    | FSharpExprPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> 
        visitObjArg f objExprOpt
    | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
        visitExpr f guardExpr; visitExpr f thenExpr; visitExpr f elseExpr
    | FSharpExprPatterns.Lambda(lambdaVar, bodyExpr) -> 
        visitExpr f bodyExpr
    | FSharpExprPatterns.Let((bindingVar, bindingExpr, debug1), bodyExpr) -> 
//        visitExpr f bindingExpr
        visitExpr f bodyExpr
    | FSharpExprPatterns.LetRec(recursiveBindings, bodyExpr) ->
//            (snd >> visitExpr f)
        List.iter (fun (z,x,c) -> visitExpr f x) recursiveBindings; visitExpr f bodyExpr
    | FSharpExprPatterns.NewArray(arrayType, argExprs) -> 
        visitExprs f argExprs
    | FSharpExprPatterns.NewDelegate(delegateType, delegateBodyExpr) -> 
        visitExpr f delegateBodyExpr
    | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) -> 
        visitExprs f argExprs
    | FSharpExprPatterns.NewRecord(recordType, argExprs) ->  
        visitExprs f argExprs
    | FSharpExprPatterns.NewAnonRecord(recordType, argExprs) ->  
        visitExprs f argExprs
    | FSharpExprPatterns.NewTuple(tupleType, argExprs) -> 
        visitExprs f argExprs
    | FSharpExprPatterns.NewUnionCase(unionType, unionCase, argExprs) -> 
        visitExprs f argExprs
    | FSharpExprPatterns.Quote(quotedExpr) -> 
        visitExpr f quotedExpr
    | FSharpExprPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> 
        visitObjArg f objExprOpt
    | FSharpExprPatterns.AnonRecordGet(objExpr, recordOrClassType, fieldInfo) -> 
        visitExpr f objExpr
    | FSharpExprPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) -> 
        visitObjArg f objExprOpt; visitExpr f argExpr
    | FSharpExprPatterns.Sequential(firstExpr, secondExpr) -> 
        visitExpr f firstExpr; visitExpr f secondExpr
    | FSharpExprPatterns.TryFinally(bodyExpr, finalizeExpr, debug1, debug2) -> 
        visitExpr f bodyExpr; visitExpr f finalizeExpr
    | FSharpExprPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr, debug1, debug2) -> 
        visitExpr f bodyExpr; visitExpr f catchExpr
    | FSharpExprPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) -> 
        visitExpr f tupleExpr
    | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) -> 
        visitExpr f decisionExpr; List.iter (snd >> visitExpr f) decisionTargets
    | FSharpExprPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) -> 
        visitExprs f decisionTargetExprs
    | FSharpExprPatterns.TypeLambda(genericParam, bodyExpr) -> 
        visitExpr f bodyExpr
    | FSharpExprPatterns.TypeTest(ty, inpExpr) -> 
        visitExpr f inpExpr
    | FSharpExprPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) -> 
        visitExpr f unionExpr; visitExpr f valueExpr
    | FSharpExprPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) -> 
        visitExpr f unionExpr
    | FSharpExprPatterns.UnionCaseTest(unionExpr, unionType, unionCase) -> 
        visitExpr f unionExpr
    | FSharpExprPatterns.UnionCaseTag(unionExpr, unionType) -> 
        visitExpr f unionExpr
    | FSharpExprPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) -> 
        visitExpr f baseCallExpr
        List.iter (visitObjMember f) overrides
        List.iter (snd >> List.iter (visitObjMember f)) interfaceImplementations
    | FSharpExprPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) -> 
        visitExprs f argExprs
    | FSharpExprPatterns.ValueSet(valToSet, valueExpr) -> 
        visitExpr f valueExpr
    | FSharpExprPatterns.WhileLoop(guardExpr, bodyExpr, debug1) -> 
        visitExpr f guardExpr; visitExpr f bodyExpr
    | FSharpExprPatterns.BaseValue baseType -> ()
    | FSharpExprPatterns.DefaultValue defaultType -> ()
    | FSharpExprPatterns.ThisValue thisType -> ()
    | FSharpExprPatterns.Const(constValueObj, constType) -> ()
    | FSharpExprPatterns.Value(valueToGet) -> ()
    | _ -> failwith (sprintf "unrecognized %+A" e)

//and visitExprs f exprs = 
//    List.iter (visitExpr f) exprs
//

let printast body =
//    System.IO.File.AppendAllText(@"C:\Temp\fsallog.txt","test:\n")
    ()
//    body |> visitExpr (fun e ->
//        sprintf "expr: %A" e
//        |> (fun f -> f |> Seq.truncate 100 )
//        |> Seq.toArray
//        |> (fun f -> System.String(f))
//        |> (fun f -> System.IO.File.AppendAllText(@"C:\Temp\fsallog.txt",f + "-----\n"))
////        |> System.Console.WriteLine
//    )
//            