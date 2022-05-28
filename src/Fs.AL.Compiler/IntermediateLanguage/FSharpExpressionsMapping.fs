module rec Fs.AL.Compiler.IntermediateLanguage.FSharpExpressionsMapping

open System
open Fs.AL.Compiler
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.IntermediateLanguage.ALContext
open Fs.AL.Compiler.Visitors
open Fs.AL.Core.ALCoreValues
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Compiler.Reflection
open ALLanguage
open Fs.AL.Core.Abstract
open Microsoft.FSharp.Reflection

module FSExpr =
    
    let groupSequentials (_curr:FSharpExpr) (_next:FSharpExpr) =
        let rec chainedSequentials acc next' =
            match next' with
            | FSharpExprPatterns.Sequential(curr,nextExpr) ->
                chainedSequentials (curr :: acc) nextExpr
            | x -> x :: acc
        let chain = chainedSequentials [_curr] _next
        chain |> List.rev

module JsonTokens =
    
    let private getFirstCast fullname =
        match fullname with
        | TypeReplacements.FullNames.FsArray -> "AsArray"
        | TypeReplacements.FullNames.String 
        | TypeReplacements.FullNames.Int32 
        | TypeReplacements.FullNames.Boolean 
        | TypeReplacements.FullNames.Double -> "AsValue"
        | TypeReplacements.FullNames.JsonValue -> "AsObject"
        | TypeReplacements.FullNames.JsonArray -> "AsArray"
        | x when x.StartsWith "Fable.JsonProvider.Generator<...>" -> "AsObject"
        | x -> failwithf $"unimplemented case %A{x}"
    
    let private getSecondCast fullname =
        match fullname with
        | TypeReplacements.FullNames.FsArray -> "___"
        | TypeReplacements.FullNames.JsonArray -> "___"
        | TypeReplacements.FullNames.String -> "AsText"
        | TypeReplacements.FullNames.Int32 -> "AsInteger"
        | TypeReplacements.FullNames.Boolean -> "AsBoolean"
        | TypeReplacements.FullNames.Double -> "AsDecimal"
        | TypeReplacements.FullNames.JsonValue -> "AsToken"
        | x when x.StartsWith "Fable.JsonProvider.Generator<...>" -> "AsToken"
        | x -> failwithf $"unimplemented case %A{x}"
        
    let private getJsonCastExpr fullTypeName =
        let _castMethod1 = fullTypeName |> getFirstCast
        let _castMethod2 = fullTypeName |> getSecondCast
        
        let firstCast = ALExpression.createMemberAccessInvocation (Identifier "_jtoken") _castMethod1 []
        
        match fullTypeName with
        |  TypeReplacements.FullNames.FsArray -> firstCast
        |  TypeReplacements.FullNames.JsonArray -> firstCast
        |  x when x.StartsWith "Fable.JsonProvider.Generator<...>" -> Identifier "_jtoken"
        | _ -> 
            ALExpression.createMemberAccessInvocation firstCast _castMethod2 []
    
    let handleSelectToken b (letBinding:FSharpMemberOrFunctionOrValue) tgt methodname args =
        //todo:json-parsing:selecttoken
        let jsonTokenName = "_jtoken"
        // declare intermediate json token if it doesnt exist
        if b.localVariables |> Seq.exists (fun f -> f.name = jsonTokenName) |> not then
            {
                ALVariable.isMutable = false
                name = jsonTokenName
                altype = Simple (SimpleType "JsonToken")
            }
            |> b.localVariables.Add
        // parse value into json token
        // AL: <tgt>.SelectToken('hello',<jsonTokenName>);
        ALExpression.createMemberAccessInvocation tgt methodname (args @ [Identifier jsonTokenName])
        |> Expression
        |> b.statements.Add
        
        let letBindingTypeFullName = (FSharpType.getFullName letBinding.FullType)
                
        let assignmentExpr =
            Assignment(
                Identifier letBinding.DisplayName,
                getJsonCastExpr letBindingTypeFullName
            )
//                        let castExpr = Assignment (letBinding)
        assignmentExpr
        |> b.statements.Add

    let handleSelectTokenReturn b (letBinding:FSharpMemberOrFunctionOrValue) tgt methodname args =
        //todo:json-parsing:selecttoken
        let jsonTokenName = "_jtoken"
        // declare intermediate json token if it doesnt exist
        if b.localVariables |> Seq.exists (fun f -> f.name = jsonTokenName) |> not then
            {
                ALVariable.isMutable = false
                name = jsonTokenName
                altype = Simple (SimpleType "JsonToken")
            }
            |> b.localVariables.Add
        // parse value into json token
        // AL: <tgt>.SelectToken('hello',<jsonTokenName>);
        ALExpression.createMemberAccessInvocation tgt methodname (args @ [Identifier jsonTokenName])
        |> Expression
        |> b.statements.Add
        
        let letBindingTypeFullName = (FSharpType.getFullName letBinding.FullType)
                
        let assignmentExpr =
            Assignment(
                Identifier letBinding.DisplayName,
                getJsonCastExpr letBindingTypeFullName
            )
//                        let castExpr = Assignment (letBinding)
        assignmentExpr
    
    let handleIntermediateCoerceCast b targetType target methodname args =
        let varname = "_var" + string b.localVariables.Count
        let varfulltype = targetType |> FSharpType.getFullName
        // declare variable
        {
            name = varname
            isMutable = false
            altype = ALType.ofFSharpType targetType }
        |> b.localVariables.Add
        
        let assigmnent = JsonTokens.handleSelectTokenToVar b varname varfulltype target methodname args
        assigmnent |> b.statements.Add
        match assigmnent with
        | Assignment(targetExpr, expression) -> 
            targetExpr,targetType
        | _ -> failwith "unhandled case"
        
        
    let handleSelectTokenToVar b (varname:string) (varfulltype:string) tgt methodname args =
        //todo:json-parsing:selecttoken
        let jsonTokenName = "_jtoken"
        // declare intermediate json token if it doesnt exist
        if b.localVariables |> Seq.exists (fun f -> f.name = jsonTokenName) |> not then
            {
                ALVariable.isMutable = false
                name = jsonTokenName
                altype = Simple (SimpleType "JsonToken")
            }
            |> b.localVariables.Add
        // parse value into json token
        // AL: <tgt>.SelectToken('hello',<jsonTokenName>);
        ALExpression.createMemberAccessInvocation tgt methodname (args @ [Identifier jsonTokenName])
        |> Expression
        |> b.statements.Add
                
        let assignmentExpr =
            Assignment(
                Identifier varname,
                getJsonCastExpr varfulltype
            )
        assignmentExpr

module FSharpOperators =
    let (|HasFSharpOperator|_|) b argExprs (s:string) : ALExpression option =  
        
        let binaryOp operator = Some (handleGenericOperator b argExprs operator)
        match s with
        | FullNameOperators.``|>`` -> Some (handleOperatorPipeRight b argExprs)    
        | FullNameOperators.not -> Some (handleOperatorNot b argExprs)    
        | FullNameOperators.failwith -> Some (handleOperatorFailwith b argExprs)    
        | FullNameOperators.``-`` -> binaryOp Subtract 
        | FullNameOperators.``+`` -> binaryOp Add 
        | FullNameOperators.``<>`` -> binaryOp NotEquals   
        | FullNameOperators.op_Equals -> binaryOp Equals  
        | FullNameOperators.op_GreaterThan -> binaryOp GreaterThan    
        | FullNameOperators.op_LessThan -> binaryOp LessThan    
        | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetArray" -> Some (handleArrayGetIndex b argExprs)    
        // cast to int
        | FullNameOperators.int ->
            match argExprs[0] with
            | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
                let alexpr = ALExpression.ofFSharpExpr b inpExpr 
                Some alexpr // just ignore the coercion
            | x ->
                failwithf $"unimplemented case %A{x}"
                None
        // cast to string
        | FullNameOperators.string ->
            match argExprs[0] with
            | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
                let alexpr = ALExpression.ofFSharpExpr b inpExpr 
                Some alexpr // just ignore the coercion
            | x ->
                failwithf $"unimplemented case %A{x}"
                None
        | _ -> None
        
    let private handleOperatorPipeRight b (argExprs:FSharpExpr list) = 
        match argExprs with
        | [arg1;arg2] ->
            let argexpr1 = ALExpression.ofFSharpExpr b arg1 // constant asdasdasd
            match ALExpression.ofFSharpExpr b arg2 with
            | FSharpLambda(newVariable, alExpression) ->
                let assignmentExpr = Assignment (Identifier newVariable.name,argexpr1)
                b.statements.Add(assignmentExpr)
                alExpression
            | _ -> failwithf $"unhandled case %A{arg2}"
        | _ -> failwithf $"unhandledcase"   
    let private handleOperatorNot b (argExprs) = 
        match argExprs with
        | [ x ] ->
            let argexpr = ALExpression.ofFSharpExpr b x
            UnaryExp (ALUnaryOperator.Not,argexpr)
        | _ ->
            let f = 5
            failwithf $"case not implemented %A{argExprs}"
    
    
    let handleGenericOperator b (argExprs:FSharpExpr list) operator =
        let a0 = ALExpression.ofFSharpExpr b argExprs[0]
        let a1 = ALExpression.ofFSharpExpr b argExprs[1]
        ALExpression.createBinary a0 operator a1
        
    let handleArrayGetIndex b (argExprs:FSharpExpr list) =
        let identifier,arraytype =
            match argExprs[0] with
            | FSharpExprPatterns.Value(v) -> Identifier v.DisplayName ,v.FullType.GenericArguments[0]
            | FSharpExprPatterns.Coerce(targetType, inpExpr) -> 
                let t = inpExpr |> ALExpression.ofFSharpExpr b
                match t with
                | FSALExpr (InvocationWithoutLastArg (target,methodname,args)) ->
                    let identfier,identifiertype = JsonTokens.handleIntermediateCoerceCast b targetType target methodname args
                    identfier,identifiertype.GenericArguments[0]
                    
                | _ -> failwithf $"unimplemented %A{t}"
                
            | _ -> failwithf $"not an array type {argExprs}"
            
        
        let idx = ALExpression.ofFSharpExpr b argExprs[1]
        
        let symtype =
            arraytype.TypeDefinition
            :> FSharpSymbol
            |> (fun f -> f.FullName)
        let d = 5
        if symtype.StartsWith "Fable.JsonProvider.Generator<...>" then
            // assign json token in let expr
            FSALExpr (InvocationWithoutLastArg(identifier,"Get",[idx]))
        else
            
        failwithf $"not implemented"
        
    let private handleOperatorFailwith b argExprs =
        (List.map (ALExpression.ofFSharpExpr b) argExprs)
        |> ALExpression.createInvocation "Error" 


module FSharpExpression =
    let handleConst (statements:ResizeArray<ALStatement>) (varDeclarations:ResizeArray<ALVariable>)
        (constValueObj:obj, constType:FSharpType) =
            // ignore units
            if constValueObj = () then ()
            else Exit (Constant constValueObj) |> statements.Add
    






module ALExpression =
    let isRefValue (exp:FSharpExpr) =
        match exp with
        | FSharpExprPatterns.Value (valueToGet) ->
            valueToGet.FullType.AbbreviatedType.TypeDefinition.FullName
                = "Microsoft.FSharp.Core.FSharpRef`1"
        | _ -> false
    let isNewObject (exp:FSharpExpr) =
        match exp with
        | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) ->  true
        | _ -> false
        
    let isLetExpr (exp:FSharpExpr) =
        match exp with
        | FSharpExprPatterns.Let(bindingExpr, nextExpr) ->  true
        | _ -> false
        
    let isThis (exp:FSharpExpr) =
        match exp with
        | FSharpExprPatterns.Value(valueToGet) ->
            valueToGet.DisplayName = "this"
        | _ -> false
    
    
    let rec ofFSharpExpr (b:ALProcedureContext) (exp:FSharpExpr) =
//        Logger.logDebug $"ofFSharpExpr: {exp.Type}"
        match exp with
        | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
            
            let d = 5
            if memberOrFunc.IsPropertySetterMethod || memberOrFunc.IsImplicitConstructor then
                let prop =
                    memberOrFunc.DisplayName
                    |> (fun f -> f.Replace("`",""))
                    |> (fun f -> if f.StartsWith ("set_") then f[4..] else f)
                    |> Identifier
                let assignTo =
                    if isThis objExprOpt.Value
                    then prop
                    else
                        let tgt = objExprOpt.Value |> ALExpression.ofFSharpExpr b
                        ALExpression.createMemberAccess tgt prop
                    
                let newval = (argExprs[0] |> ALExpression.ofFSharpExpr b)
                let stat = Assignment(assignTo,newval)
                b.statements.Add(stat)
                FSALExpr (Ignore)
            else
            
//            let rootfullname = memberOrFunc :> FSharpSymbol
            let f = 5
            
            let fullname1 = memberOrFunc.FullName
            let d = 5
            //let alExprs = argExprs |> List.map (ALExpression.ofFSharpExpr throwawayctx)
            //let altgt = objExprOpt |> Option.map (ALExpression.ofFSharpExpr throwawayctx)
            match fullname1 with
            | TypeReplacements.HasReplacementFunction (ofFSharpExpr b) objExprOpt memberOrFunc argExprs result ->
                let replacement = result (ofFSharpExpr b) objExprOpt memberOrFunc argExprs
                match replacement with
                | FSALExpr (InvocationWithoutLastArg (tgt,methodname,args )) ->
                    // response.details.profile
                    let test = 5
//                    failwithf $"%A{test}"
                    replacement
                | _ -> replacement
//                    JsonTokens.handleSelectToken b letBinding tgt methodname args
                    // 
            | FSharpOperators.HasFSharpOperator b argExprs result -> result
            | x when objExprOpt.IsSome ->
                // TODO: handle new union case
                // default
                
                let func =
                    // remove get_ prefix
                    if memberOrFunc.IsPropertyGetterMethod then
                        memberOrFunc.DisplayName
                        |> (fun f -> f.Replace("`",""))
                        |> (fun f -> if f.StartsWith ("get_") then f[4..] else f)
                        |> Identifier
                    else Identifier memberOrFunc.DisplayName
                let args =
                    argExprs
                    |> List.where (fun f ->
                        match f with
                        | FSharpExprPatterns.NewUnionCase x -> false
                        | _ -> true
                    )
                    |> List.map (ofFSharpExpr b)
                
                let handleDefault() =
                    let target = objExprOpt.Value |> ofFSharpExpr b
                    let d = 5
                    match objExprOpt.Value |> isRefValue, memberOrFunc.IsPropertyGetterMethod, memberOrFunc.IsMemberThisValue with
                    | true,_,_ -> target // ref.Value
                    | _, true,true -> ALExpression.createMemberAccess target func // property
                    | _, true,_ -> ALExpression.createMemberAccess target func // property
                    | _ -> ALExpression.createMemberAccessInvocation target memberOrFunc.DisplayName args
                
                match objExprOpt.Value with
                | FSharpExprPatterns.Value(value) ->
                    // this.<member> reference
                    let t = 5
                    match value.IsMemberThisValue, memberOrFunc.IsPropertyGetterMethod with
                    | true,false ->
                        ALExpression.createInvocation memberOrFunc.DisplayName args
//                        ALExpression.createMemberAccessInvocation (Identifier "Rec") memberOrFunc.DisplayName args // TODO: only for tables 
                    | true,true ->
                        func
//                        ALExpression.createMemberAccess (Identifier "Rec") func
                    | _ ->  handleDefault()  
                    
                | _ ->
                    // rest
                    handleDefault()
            | me ->
                // edge cases
                match me with
                | "FSharp.Data.Runtime.JsonRuntime.GetPropertyPacked"
                | "FSharp.Data.Runtime.JsonRuntime.ConvertArray"
                | "FSharp.Data.Runtime.BaseTypes.JsonDocument.Create"
                | "FSharp.Data.Runtime.JsonRuntime.GetPropertyPackedOrNull" -> // jsonprovider get property by name
//                    Invocation (Binary  (MemberAccess (argExprs[0] |> ofFSharpExpr,argExprs[1]|> ofFSharpExpr)))
                    // todo: typeprovider
                    let t = 5
                    let target = argExprs[0] |> (ofFSharpExpr b)
                    let arg = argExprs[1]|> (ofFSharpExpr b)
                    if b.localVariables |> Seq.isEmpty |> not then
                        ALExpression.createMemberAccessInvocation
                            target ("SelectToken") [arg;Identifier (b.localVariables |> Seq.last).name]
                    else Identifier "aaaaaaaaaaaaaaaaaaaa"
//                | "Fs.AL.Core.ALSimpleValues.ReadFrom" ->
                | "Microsoft.FSharp.Core.Operators.ignore" -> FSALExpr Ignore
                | "Microsoft.FSharp.Collections.Array.length" ->
                    match argExprs with
                    | [FSharpExprPatterns.Value(v)] ->
                        match ALType.ofFSharpType v.FullType with
                        | Simple (SimpleType "JsonArray") ->
                            match argExprs with
                            | [ x ] ->
                                let alexp =
                                    ALExpression.createMemberAccess
                                        (Identifier v.DisplayName)
                                        (Identifier "Count")
                                alexp
                            | _ ->                                 
                             
                                let t1 = 5
                                failwith "wait"
                        | v ->
                            let t2 = 5
                            failwith "wait"
                    | _ -> 
                        FSALExpr Ignore
                // AL Static method
                | x when
                    objExprOpt.IsNone
                    &&
                    (memberOrFunc.ApparentEnclosingEntity |> FSharpEntity.isALObject
                    || memberOrFunc.ApparentEnclosingEntity.IsFSharpModule)
                 ->
                    
                    let ent = memberOrFunc.ApparentEnclosingEntity
                    let isSingleInstCodeunit = ent |> FSharpEntity.hasAttribute<ALSingleInstanceCodeunit>
                    let isStaticReference = 
                        if isSingleInstCodeunit
                            then Some (ALType.Complex (Codeunit ent.DisplayName))
                        else
                            let at = Type.GetType(ent.FullName)
                            let typebyctor = ent.MembersFunctionsAndValues[0]
                            let gotType = FSharpEntity.tryGetType ent
                            let isfstype = gotType |> FSharpType.isInheritedFromALType
                            Some (ALType.ofFSharpType gotType.Value)
                            
                    match isStaticReference with
                    | None -> failwith "could not resolve type" 
                    | Some reftype ->
                        let dispName = "_static_"+ent.DisplayName
                        let staticVar =
                            {
                                isMutable = false
                                name = dispName
                                altype = reftype
                            }
                        b.localVariables.Add staticVar    
                        
                        let expr =
                            ALExpression.createMemberAccessInvocation
                                (Identifier dispName)
                                memberOrFunc.DisplayName
                                (argExprs |> List.map (ofFSharpExpr b))
                        expr
                | _ ->
                    if memberOrFunc.CompiledName = "op_Implicit" then argExprs[0] |> ofFSharpExpr b
                    else
                    // inline lambda
                    match argExprs[0] with 
                    | FSharpExprPatterns.Lambda x ->
                        let lamb = ofFSharpExpr b argExprs[0]
                        let arg = argExprs[1] |> (ofFSharpExpr b)
                        NaryExpression (Invocation((lamb,[arg])))
                    | _ ->
                        let f = 5
                        failwithf $"case not implemented %A{memberOrFunc.FullName}"
        | FSharpExprPatterns.Lambda(lambdaVar, bodyExpr) ->
            let block = Block
            let alExpr = bodyExpr |> ofFSharpExpr b
            let vname = $"v{b.localVariables.Count}"
            let newVar = {
                isMutable = false
                name = vname
                altype = ALType.ofFSharpType lambdaVar.FullType
//                altype = ALType.ofFSharpType bodyExpr.Type
            }
            b.localVariables.Add(newVar)
//            let e2 = alExpr |> ALExpression.Invocation.changeArgument (Identifier vname)
//            FSharpLambda (newVar,e2)
//            FSALExpr Ignore
            alExpr
            
        | FSharpExprPatterns.Value(valueToGet) ->
            
            Identifier valueToGet.DisplayName
        | FSharpExprPatterns.Const(constValueObj, constType) -> Constant constValueObj
        | FSharpExprPatterns.DefaultValue defaultType -> FSALExpr (Ignore) // json parsing
        | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
            // passing lambdas
            let exp = inpExpr |> ofFSharpExpr b
            match exp with
            | FSALExpr (InvocationWithoutLastArg (target,methodname,args)) -> //id response, selecttoken, constant naem
//                let finalcast = 
//                    match targetType |> FSharpType.getFullName with
//                    | TypeReplacements.FullNames.String -> "AsText"
//                    | x when x.StartsWith "Fable.JsonProvider.Generator<...>" ->
//                        ""
//                    | _ -> failwithf $"unimplemented"
                    
                if methodname = "SelectToken" then
                    let varname = "_var" + string b.localVariables.Count
                    let varfulltype = targetType |> FSharpType.getFullName
                    // declare variable
                    {
                        name = varname
                        isMutable = false
                        altype = ALType.ofFSharpType targetType }
                    |> b.localVariables.Add
                    
                    let assigmnent = JsonTokens.handleSelectTokenToVar b varname varfulltype target methodname args
                    assigmnent |> b.statements.Add
                    match assigmnent with
                    | Assignment(targetExpr, expression) -> 
                        targetExpr
                        
                    | _ -> failwithf $"unimplemented"
                else failwith "unimplemented"
            | _ -> failwith "unimplemented"
                
//            let fsdf = 5
//            exp
        
        | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) ->
            //TODO: this is an identity assignment for optimized functions, remove it
            let d = 5
            Identifier "returnVal"
//            Ignore
        | FSharpExprPatterns.Sequential(firstExpr, secondExpr) ->
            let s = 5
            let grouped = FSExpr.groupSequentials firstExpr secondExpr
//            match b.expressionContext with
//            | ExpressionContext.Sequential chain ->
//                let blockchain =
//                    Block grouped
//                
//            | ExpressionContext.None ->
//                ()
//
            let mapped = grouped |> List.map (ofFSharpExpr b >> ALStatement.ofExpression)
            let block = Block mapped
//            let alexpr = firstExpr |> ofFSharpExpr b
//            let expr = secondExpr |> ofFSharpExpr b
//            match firstExpr with
//            | FSharpExprPatterns.Const(constValueObj, constType)
//                when constValueObj = null -> () //newline?
//            | _ ->
//                b.statements.Add(Expression alexpr) // add first
            FSALExpr (StatementExpr block) 
        | FSharpExprPatterns.Let((bindingVar, bindingExpr, debug1), bodyExpr) ->
            //todo:json-parsing:start
            // inner let expr
            let last =  b.localVariables |> Seq.last
            let varToAdd =
                {
                    isMutable = false
                    name = bindingVar.DisplayName
                    altype = ALType.ofFSharpType bindingVar.FullType
                }
            if last = varToAdd then bodyExpr |> ofFSharpExpr b else
            let assignedTo = Identifier bindingVar.DisplayName
            let assignment = bindingExpr |> ofFSharpExpr b
            match assignment with
            | FSALExpr (InvocationWithoutLastArg (target,method,args)) ->
                JsonTokens.handleSelectTokenReturn
                    b bindingVar target method args
                |> (fun f -> FSALExpr (FSALMappingExpr.StatementExpr f))
//                FSALExpr Ignore
            | _ ->                
            let innerstatement = Assignment (assignedTo, assignment)
            // json := 'sometext';
            b.statements.Add (innerstatement)
            // declare the json token variable
            varToAdd |> b.localVariables.Add
            let a1 = 5
            bodyExpr |> ofFSharpExpr b
            
        | FSharpExprPatterns.ValueSet(valToSet, valueExpr) ->
            let t = 5
            let exp = valueExpr |> ALExpression.ofFSharpExpr b
            Assignment(Identifier valToSet.DisplayName,exp)
            |> StatementExpr
            |> FSALExpr
            
//            Assignment valueExpr |> ALExpression.ofFSharpExpr b
//            Ignore
        | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> //v
            let guardStatement = guardExpr |> ALExpression.ofFSharpExpr b
            let thenStatement = thenExpr |> ALExpression.ofFSharpExpr b |> Expression
            let elseStatement = 
                match elseExpr |> ALExpression.ofFSharpExpr b with
                | Constant null -> None
                | elseStatement -> Some (Expression elseStatement)
            IfStatement( guardStatement,thenStatement,elseStatement )
            |> StatementExpr
            |> FSALExpr
        | FSharpExprPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) ->
            DecisionExpr decisionTargetIdx
            |> FSALExpr
        | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) ->
            // todo: needs refactoring
            let createdExpression = decisionExpr |> ALExpression.ofFSharpExpr b
            let expressionWithDecisions =
                createdExpression
                |> ALExpression.replaceDecisionExpressions
                       true
                       (System.Collections.Generic.Dictionary())
                       ((b.localVariables |> Seq.last).name)
                       (ALExpression.ofFSharpExpr b)
                       decisionTargets
            expressionWithDecisions
        | x -> failwithf $"%A{x}"
        

    
   
   

    
        
[<RequireQualifiedAccess>]        
module ALExpressionVisitor =
    let rec visit fn (alstat:ALExpression) =
        match alstat with
        | Binary (a1,op,a2) ->
            visit fn a1
            visit fn a2
        | NaryExpression alNaryExpression ->
            match alNaryExpression with
            | Invocation(alExpression, alExpressions) ->
                visit fn alExpression
                alExpressions |> Seq.iter (visit fn)
            | _ -> failwith "unimplemented case"
        | UnaryExp (op,a1) -> visit fn a1
        | FSharpLambda(newVariable, alExpression) ->
            visit fn alExpression
        | _ ->
            let a = 5
            ()

  
[<RequireQualifiedAccess>]        
module ALStatementVisitor =
    let rec visit fn (alstat:ALStatement) =
        let dd = 5
        match alstat with
        | Assignment(alExpression, expression) ->
            ALExpressionVisitor.visit fn alExpression
            ALExpressionVisitor.visit fn expression
        | Expression alExpression ->
            ALExpressionVisitor.visit fn alExpression
        | Sequence(alStatement, statement) ->
            visit fn alStatement
            visit fn statement
        | Block alExpressions -> alExpressions |> List.iter (visit fn) 
        | IfStatement(alExpression, alStatement, alStatementOption) ->
            ALExpressionVisitor.visit fn alExpression
            visit fn alStatement
            alStatementOption |> Option.iter (visit fn)
        | Exit alExpression -> ()