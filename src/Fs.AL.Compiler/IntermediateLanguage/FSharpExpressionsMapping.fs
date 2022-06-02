module rec Fs.AL.Compiler.IntermediateLanguage.FSharpExpressionsMapping

open System
open System.Diagnostics
open Fs.AL.Compiler
open Fs.AL.Compiler.CompilerServices
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.IntermediateLanguage.ALContext
open Fs.AL.Compiler.Visitors
open Fs.AL.Core.ALCoreValues
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Compiler.Fullname
open ALLanguage
open Fs.AL.Core.Abstract
open Microsoft.FSharp.Reflection
open System.Reflection

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
        
    let getJsonCastExpr (identifier:ALExpression) (fullTypeName:string) = //letBindVarJsonName
        let _castMethod1 = fullTypeName |> getFirstCast
        let _castMethod2 = fullTypeName |> getSecondCast
        let firstCast = ALExpression.createMemberAccessInvocation identifier _castMethod1 []
        match fullTypeName with
        |  TypeReplacements.FullNames.FsArray -> firstCast
        |  TypeReplacements.FullNames.JsonArray -> firstCast
        |  x when x.StartsWith "Fable.JsonProvider.Generator<...>" -> identifier
        | _ -> 
            ALExpression.createMemberAccessInvocation firstCast _castMethod2 []
    
module FSharpOperators =
    let (|HasFSharpOperator|_|) b argExprs (s:string) : ALExpression option =  
        let binaryOp operator = Some (handleGenericOperator b argExprs operator)
        match s with
        | Operators.op_PipeRight -> Some (handleOperatorPipeRight b argExprs)    
        | Operators.not -> Some (handleOperatorNot b argExprs)    
        | Operators.failwith -> Some (handleOperatorFailwith b argExprs)    
        | Operators.op_Subtract -> binaryOp Subtract 
        | Operators.op_Add -> binaryOp Add 
        | Operators.op_NotEquals -> binaryOp NotEquals   
        | Operators.op_Equals -> binaryOp Equals  
        | Operators.op_GreaterThan -> binaryOp GreaterThan    
        | Operators.op_LessThan -> binaryOp LessThan    
        | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetArray" -> Some (handleArrayGetIndex b argExprs)    
        // cast to int
        | Operators.int ->
            match argExprs[0] with
            | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
                let alexpr = ALExpression.ofFSharpExpr b inpExpr 
                Some alexpr // just ignore the coercion
            | x ->
                failwithf $"unimplemented case %A{x}"
                None
        // cast to string
        | Operators.string ->
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
                let nextexpr = ALStatement.ofExpression alExpression
                Sequence(assignmentExpr,nextexpr )
                |> ALExpression.ofALStatement
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
                    failwith "needs reimplementation"
                    
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


module ALExpressionTranslation =
    
    
    
    
    let translateDecisionTreeSuccess (b:ALProcedureContext) (exp:FSharpExpr) =
        match exp with
        | FSharpExprPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) ->
            let decisionCtx =
                b.expressionContext
                |> Seq.choose (fun f ->
                    match f with
                    | ALExprContext.DecisionTree(decisionExpr, alExpressions) ->
                        alExpressions |> Some
                    | _ -> None
                )
                |> Seq.last
            let chosenDecision = decisionCtx[decisionTargetIdx]
            chosenDecision
        | _ ->  failwith "invalid expression"
            
        
        
    let translateValueSet (b:ALProcedureContext) (exp:FSharpExpr) =
        match exp with
        | FSharpExprPatterns.ValueSet(valToSet, valueExpr) ->
            let t = 5
            let exp = valueExpr |> ALExpression.ofFSharpExpr b
            Assignment(Identifier valToSet.DisplayName,exp)
            |> StatementExpr
            |> FSALExpr
        | _ -> failwith "invalid expression"
    let translateLet (b:ALProcedureContext) (exp:FSharpExpr) =
        match exp with
        | FSharpExprPatterns.Let((letBindingVar, letBindingExpr, debug1), letBodyExpr) ->
            if b.identifier = "assignment" then
                let breakpoint = 1
                ()

//            let context = ALExprContext.LetBinding (letBindingVar,letBindingExpr) 
//            b.expressionContext.Add(context)
            let f = 5
            // determine let expression kind
            let bindingCtx,bindings =
                match letBindingExpr with
                // empty variable declaration
                | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) ->
                    match argExprs with
                    | [  ] when objType.IsConstructor -> LetExprKind.Declaration,letBindingExpr |> ALExpression.ofFSharpExpr b
                    | [ x ] when objType.IsConstructor && x.Type |> FSharpType.isUnit -> LetExprKind.Declaration,letBindingExpr |> ALExpression.ofFSharpExpr b
                    | _ -> failwith ""
                // constructor assignments
                | FSharpExprPatterns.Let((bindingVarInner, _, _), _) -> 
                    let newIdentifier = "." + letBindingVar.DisplayName
                    ALExprContext.usingCtx (ALExprContext.Constructor newIdentifier) b 
                        (fun f ->
                        LetExprKind.Constructor,letBindingExpr |> ALExpression.ofFSharpExpr b 
                    )
                // decision tree expr
                | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) ->
                    let result = translateDecisionTree b letBindingExpr
                    let v = 5
                    LetExprKind.Decision,letBindingExpr |> ALExpression.ofFSharpExpr b
                // jsonprovider casts
                | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
                    match inpExpr with
                    | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
                        match memberOrFunc.FullName with
                        | "Fable.JsonProvider.jsonParse" ->
                            let expkind = LetExprKind.TypeProvider (memberOrFunc,targetType,argExprs)
                            LetExprKind.Invocation ,
                            ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,expkind)) b (fun f ->
                                letBindingExpr |> ALExpression.ofFSharpExpr b
                            )
                        | "Fable.JsonProvider.getProp" ->
                            let jsonprovtest = 11111
                            let expkind = LetExprKind.TypeProvider (memberOrFunc,targetType,argExprs)
                            ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,expkind)) b (fun f ->
                                LetExprKind.TypeProvider (memberOrFunc,targetType,argExprs),
                                letBindingExpr |> ALExpression.ofFSharpExpr b
                            )
                            
                        | _ -> failwith "unimplemented coerce"
                    | _ -> failwith "unimplemented coerce"
                // TODO: casting
//                | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
//                    let symbol = memberOrFunc :> FSharpSymbol
//                    let test1 = 1
//                    match symbol.FullName with 
//                    | FSharpOperators.HasFSharpOperator b argExprs result ->
//                        let resu1 = result
//                        LetExprKind.Normal,letBindingExpr |> ALExpression.ofFSharpExpr b                        
//                    | _ -> failwith "test"
////                    
                | _ -> LetExprKind.Normal,letBindingExpr |> ALExpression.ofFSharpExpr b
 
            // declare AL variable
            let varToAdd = {
                isMutable = false
                name = letBindingVar.DisplayName
                altype = ALType.ofFSharpType letBindingVar.FullType
            }
            let declareVariable() =
                let last =  b.localVariables |> Seq.tryLast
                if last <> Some varToAdd then
                    //TODO: can currently add duplicates
                    varToAdd |> b.localVariables.Add
                
            // next expr
            let fsalBodyExpr() = letBodyExpr |> ALExpression.ofFSharpExpr b |> ALStatement.ofExpression
            let test2 = 5
            match bindingCtx, letBindingVar.IsCompilerGenerated with
            | LetExprKind.Invocation,_ ->
                declareVariable()
                Sequence(bindings |> ALStatement.ofExpression,fsalBodyExpr()) |> ALExpression.fromALStatement
            | LetExprKind.TypeProvider(memberOrFunctionOrValue, targetType, exprs),_ ->
                ()
                let t1 = ""
                let jsonCasting = bindings |> ALStatement.ofExpression
                let bodyExpr = fsalBodyExpr() 
                Sequence(jsonCasting,bodyExpr)
                |> ALExpression.fromALStatement
//                failwith "test"
            | LetExprKind.Constructor,_ ->
                declareVariable() // ctor return value name
//                let ctorCtx = ALExprContext.getCtorCtx b |> Option.get
                let bindingsExpr = bindings |> ALStatement.ofExpression
                let test = 5
                // declare variable with different name
                let blockStatement,ctorReturnValue =
                    match bindingsExpr with
                    | Block alStatements ->
                        alStatements[..alStatements.Length - 2]
                        |> List.where (fun f ->
                            match f with
                            | ALStatement.Expression (Constant null) -> false 
                            | _ -> true
                        )
                        |> Block , // assignment statements
                        // assign returnVal
                        Assignment(Identifier letBindingVar.DisplayName,alStatements[alStatements.Length - 1] |> ALExpression.ofALStatement)
                    | _ -> failwith "invalid constructor expr"
                let f1233123 = 5
                
                let ctorInnerVariable =
                    match ctorReturnValue with
                    | Assignment(_, Identifier s) -> s 
                    | _ -> failwithf $"%A{ctorReturnValue}"
                
                Sequence(
                    blockStatement
                    |> ALStatement.toStatements
                    |> List.map (fun stmt -> stmt |> ALStatement.withAssignmentIdentifier ctorInnerVariable )
                    |> ALStatement.toBlock,
                    Sequence(
                        ctorReturnValue |> ALStatement.withAssignmentTarget (Identifier ctorInnerVariable),
                        fsalBodyExpr()
                    )
                )
                |> ALExpression.fromALStatement
//                Sequence(s1,fsalBodyExpr()) |> ALExpression.fromALStatement
            | _, true ->
                // compiler-generated constructor statements // -- before constructor statement
//                b.expressionContext.Remove(context) |> ignore // remove let statement context
                
                match ALExprContext.getCtorCtx b with
                | None ->
                    let assignmentsBlock =
                        varToAdd |> b.localVariables.Add
                        let asdasd = 5
                        let statements = fsalBodyExpr() |> ALExpression.fromALStatement
                        let assignStatements,returnStatement =
                            ALStatement.collectSequencesLast (statements |> ALStatement.ofExpression)
                            |> (fun (seqStatements,returnStatement) ->
                                seqStatements
                                |> ALStatement.withoutNullStatements
                                |> List.map (fun statement ->
                                    statement //|> ALStatement.withAssignmentIdentifier ctorCtx
                                ),
                                returnStatement
//                                Identifier ctorCtx |> ALStatement.ofExpression
                            )
                        Block
                            (assignStatements @
                                [
                                    // constructor expr return value
                                    returnStatement
                                ])
                        |> ALExpression.fromALStatement
                    assignmentsBlock
                | Some ctorCtx ->   
                    let assignmentsBlock =
                        { varToAdd with name = ctorCtx} |> b.localVariables.Add
                        let asdasd = 5
                        let statements = fsalBodyExpr() |> ALExpression.fromALStatement
                        let assignStatements,returnStatement =
                            ALStatement.collectSequencesLast (statements |> ALStatement.ofExpression)
                            |> (fun (seqStatements,returnStatement) ->
                                seqStatements
                                |> ALStatement.withoutNullStatements
                                |> List.map (fun statement ->
                                    statement |> ALStatement.withAssignmentIdentifier ctorCtx
                                ),
                                Identifier ctorCtx |> ALStatement.ofExpression
                            )
                            
                        let asdasddfgfd =5
                        Block
                            (assignStatements @
                                [
                                    // constructor expr return value
                                    returnStatement
                                ])
                        |> ALExpression.fromALStatement
               
                    assignmentsBlock
            | LetExprKind.Declaration,false ->
                declareVariable()
                // skip empty bindings
//                b.expressionContext.Remove(context) |> ignore
                let result = fsalBodyExpr() |> ALExpression.fromALStatement
                result
            | _ ->
                declareVariable()
                // unwrap preceding statements
                let precedingStatements,remainingStatement =
                    ALStatement.unwrapExpression [] bindings
                
                match precedingStatements with
                | [] ->   
                    let assignmentStatement =
                        Assignment(
                            Identifier letBindingVar.DisplayName,
                            bindings                    
                        )
                    Sequence(assignmentStatement,fsalBodyExpr()) |> ALExpression.fromALStatement
                | _ ->
                    let statements =
                        Sequence(
                            Block precedingStatements,
                            Assignment(
                                Identifier letBindingVar.DisplayName,
                                remainingStatement 
                            )
                        )
                    Sequence(statements,fsalBodyExpr()) |> ALExpression.fromALStatement
                    
                
        | _ -> failwith "invalid expression"
    
    let translateDecisionTree b expr : ALExpression =
        match expr with 
        | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) ->
            
            let context =
                let containingLet =
                    b.expressionContext
                    |> Seq.choose (fun f ->
                        match f with
                        | ALExprContext.LetBinding(valexpr, bindingExpr) ->
                            valexpr |> Some | _ -> None )
                    |> Seq.tryLast
                let dafdsf =5 
                let targets =
                    decisionTargets
                    |> List.map (fun (args,expr) ->
                        if args.Length > 0
                            then failwith "args"
                        let result =  expr |> ALExpression.ofFSharpExpr b
                        match containingLet with
                        | Some v ->
                            let preceding,last = ALStatement.collectSequencesLast (result |> ALStatement.ofExpression)
                            let finalassignment = Assignment(Identifier v.DisplayName,last |> ALExpression.ofALStatement) 
                            match preceding with
                            | [] -> finalassignment |> ALExpression.fromALStatement 
                            | _ -> Block (preceding @ [finalassignment]) |> ALExpression.fromALStatement 
                               
                        | None -> result 
                    )
            
                let context2 = ALExprContext.DecisionTree (decisionExpr,targets)
                context2
            
            let res =
                ALExprContext.usingCtx context b
                    (fun f ->
                        let createdExpression = decisionExpr |> ALExpression.ofFSharpExpr b
                        let asd1 = 5
                        createdExpression
                )
            
            // pattern match case is different in AL
            
            
            // todo: this may add unnecessary variables
            // figure out a way to evaluate once
//                let bcopy =
//                    { b with
//                        localVariables = ObservableCollection()
//                        statements = ObservableCollection() }
//                                
            // assign decision targets
            let f1 = 5
            // todo: get let binding context
            
            let assignedLetVar =
                match b.expressionContext |> Seq.last with
                | ALExprContext.LetBinding(memberOrFunctionOrValue, bindingExpr) ->
                    Some memberOrFunctionOrValue
                | _ -> None
             
//            let expressionWithDecisions =
//                res
//                |> ALExpression.replaceDecisionExpressions
//                       true
//                       (System.Collections.Generic.Dictionary())
//                       assignedLetVar.Value.DisplayName
////                       letBinding.DisplayName
//                       (ALExpression.ofFSharpExpr b)
//                       decisionTargets
//            ALStatement.ofExpression (Identifier "test")
//            |> StatementExpr
//            |> FSALExpr
            res
            
        | _ -> failwith "invalid expression"
module ALExpression =
    let handleConst (statements:ResizeArray<ALStatement>) (varDeclarations:ResizeArray<ALVariable>)
        (constValueObj:obj, constType:FSharpType) =
            // ignore units
            if constValueObj = () then ()
            else Exit (Constant constValueObj) |> statements.Add
    
    let getConstValue (exp:ALExpression) =
        match exp with
        | Constant o -> o 
        | _ -> failwith "not a constant expresison"
    

    




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
        
        let toExpr (exp:ALStatement) = exp |> StatementExpr |> FSALExpr 
        let toStatement (exp:ALExpression) = exp |> ALStatement.ofExpression 
        Logger.logDebug $"fsharpexpr: %s{FSExpr.getPatternName exp}"
        
        match exp with
        | FSharpExprPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) ->
            let result = ALExpressionTranslation.translateDecisionTreeSuccess b exp
            result 
            
        | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) ->
            ALExpressionTranslation.translateDecisionTree b exp
        | FSharpExprPatterns.Let((bindingVar, bindingExpr, debug1), bodyExpr) ->
            // fail if inner let expr
            match bindingExpr with
            | FSharpExprPatterns.Let((lBindVar, _, _), _) ->
                match lBindVar.IsCompilerGenerated with
                | true -> ()
                | false -> 
                    failwith
                        ([
                           "inner let expressions are currently not supported: "
                           $"line:{lBindVar.DeclarationLocation.StartLine}"
                           $"file:{lBindVar.DeclarationLocation.FileName}"
                        ] |> String.concat "\n")
            | _ -> () 
            let result = ALExpressionTranslation.translateLet b exp
            result 
            
        | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
            let sym_memberOrFunc = memberOrFunc :> FSharpSymbol
            let d = 5
            if memberOrFunc.IsPropertySetterMethod || memberOrFunc.IsImplicitConstructor then
                let debug = 5
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
                stat |> ALExpression.fromALStatement 
            else
            
                let rootfullname = memberOrFunc :> FSharpSymbol
                let f = 5
            
                match memberOrFunc.FullName with
                | TypeReplacements.HasReplacementFunction (ofFSharpExpr b) objExprOpt memberOrFunc argExprs result ->
                    let r = 5
                    let replacement = result (ofFSharpExpr b) objExprOpt memberOrFunc argExprs
                    match replacement with
                    | NaryExpression (Invocation(Binary(_tgt, _memberaccess, _member), _args)) ->
                        match _tgt with
                        | Identifier "_fs_enumerator" ->
                            ALProcedureContext.ensureHasVariable b
                                {
                                ALVariable.isMutable = false
                                name = "_fs_enumerator"
                                altype = ALType.Complex (Codeunit "FS_Enumerator")
                                }
                            replacement
                        | _ -> replacement
                    | FSALExpr (InvocationWithoutLastArg (tgt,methodname,args )) ->
                        replacement
                    | _ -> replacement
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
                    | "Microsoft.FSharp.Core.Operators.ref" -> FSALExpr Ignore
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
        | FSharpExprPatterns.Const(constValueObj, constType) -> Constant constValueObj // json parsing
        | FSharpExprPatterns.DefaultValue defaultType -> FSALExpr (Ignore)

        //            let fsdf = 5
//            exp

        | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
            // passing lambdas
            let coerc = 1
            let letBindContext = (ALExprContext.getLetBindingCtx b)
            let exp = inpExpr |> ofFSharpExpr b
            match letBindContext,exp with
            | None, FSALExpr (InvocationWithoutLastArg (Identifier name,methodname,args)) ->
                let targetJsonProp : string  = args[0] |> ALExpression.getConstValue |> unbox 
                let intermediaryVarName = ".json." + targetJsonProp + (string b.localVariables.Count)
                let targetVarName = "@" + targetJsonProp + (string b.localVariables.Count)
                // declare intermediary var
                {   name = intermediaryVarName
                    isMutable = false
                    altype = Simple (SimpleType "JsonToken") } //ALType.ofFSharpType targetType
                |> b.localVariables.Add
                // select token to intermediary var
                let selectToken =
                    ALExpression.createMemberAccessInvocation (Identifier "_jtoken") methodname (args @ [Identifier intermediaryVarName])
                    |> Expression
                // declare target var
                {   name = targetVarName
                    isMutable = false
                    altype = ALType.ofFSharpType targetType } //ALType.ofFSharpType targetType
                |> b.localVariables.Add
                // assign var from intermediary
                let letBindingTypeFullName = (FSharpType.getFullName targetType)
                let assignmentExpr =
                    Sequence(
                        selectToken,
                        Assignment(
                            Identifier targetVarName,
                            JsonTokens.getJsonCastExpr (Identifier intermediaryVarName) letBindingTypeFullName
                        )
                    )
                    
                let vsadsadasd = 1
                let test = 1
                Sequence (assignmentExpr,Identifier targetVarName |> ALStatement.ofExpression) |> ALExpression.fromALStatement
            | Some (letBindVar,bindingKind), FSALExpr (InvocationWithoutTarget (methodname,args)) ->
                ALProcedureContext.ensureHasJTokenVariable b
//                response.ReadFrom(json);
//                response.SelectToken('details',_jtoken);
//                details := _jtoken;
                
                let statement =
                    match bindingKind with
                    | LetExprKind.TypeProvider(memberOrFunctionOrValue, targetType, exprs) ->
                        let test1 = 1
                        // jsonParse
                        ALExpression.createMemberAccessInvocation (Identifier letBindVar.DisplayName) methodname args
                        
                    | _ -> failwith "invalid binding"
                    
//                failwith "todo"
//                ALExpression.createMemberAccessInvocation (Identifier target.DisplayName) methodname args
                statement
                
            | Some (letBindVar,bindingKind), FSALExpr (InvocationWithoutLastArg (target,methodname,args)) -> // target "reader", args [constant firstname]
                let letBindVarJsonName = ".json." + letBindVar.DisplayName
                match bindingKind with
                | LetExprKind.TypeProvider(memberOrFunctionOrValue, targetType, exprs) -> //memberfuncvalue : getprop, exprs [reader, 'firstname']
                    // declare intermediary var
                    {   name = letBindVarJsonName
                        isMutable = false
                        altype = Simple (SimpleType "JsonToken") } //ALType.ofFSharpType targetType
                    |> b.localVariables.Add
                    // select token to intermediary var
                    let memberAccess =
                        ALExpression.createMemberAccessInvocation (Identifier "_jtoken") methodname (args @ [Identifier letBindVarJsonName])
                        |> Expression
                    // declare target var
                    {   name = letBindVar.DisplayName
                        isMutable = false
                        altype = ALType.ofFSharpType targetType } //ALType.ofFSharpType targetType
                    |> b.localVariables.Add
                    // assign var from intermediary
                    let letBindingTypeFullName = (FSharpType.getFullName letBindVar.FullType)
                    let assignmentExpr =
                        Assignment(
                            Identifier letBindVar.DisplayName,
                            JsonTokens.getJsonCastExpr (Identifier letBindVarJsonName) letBindingTypeFullName
                        )
                    Sequence (memberAccess,assignmentExpr) |> ALExpression.fromALStatement
                | _ ->
                failwith "unimplemented"
                    
                if methodname = "SelectToken" then
                    let varname = "_var" + string b.localVariables.Count
                    let varfulltype = targetType |> FSharpType.getFullName
                    // declare variable
//                    {
//                        name = varname
//                        isMutable = false
//                        altype = ALType.ofFSharpType targetType }
//                    |> b.localVariables.Add
                    failwith "needs reimplementation"
//                    let assigmnent = JsonTokens.handleSelectTokenToVar b varname varfulltype target methodname args
//                    match assigmnent with
//                    | Assignment(targetExpr, expression) ->
//                        let next = targetExpr |> ALStatement.ofExpression
//                        Sequence(assigmnent,next)
//                        |> ALExpression.fromALStatement
//                    | Sequence(
//                        Expression alExpression, // assign jtoken
//                        Assignment(targetExpr, expression)) -> // cast jtoken and assign
//                        let ffasdasd = 5
//                        Sequence(Expression alExpression,Assignment(targetExpr, expression))
//                        |> ALExpression.fromALStatement
//                    | _ -> failwithf $"unimplemented"
                else failwith "unimplemented"
            | _ -> failwith "unimplemented"
        //            Ignore
        | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) ->
            //TODO: this is an identity assignment for optimized functions, remove it
            let d = 5
            Identifier "returnVal"
        | FSharpExprPatterns.Sequential(firstExpr, secondExpr) ->
//            firstExpr |> b.statements.Add
//            let grouped = FSExpr.groupSequentials firstExpr secondExpr
//            let mapped = grouped |> List.map (ofFSharpExpr b >> ALStatement.ofExpression)
//            printfn $"mappedlen:{mapped.Length}"
//            let bugg2 = 5
//            let codeblock = Block mapped
            let fsalFirst = firstExpr |> ofFSharpExpr b |> toStatement
//            fsalFirst |> b.statements.Add // Added 4 times
            let snd = 5
            let fsalSecond = secondExpr |> ofFSharpExpr b |> toStatement
            Sequence(fsalFirst,fsalSecond) |> toExpr
//            let statement = Sequence(fsalFirst,fsalSecond)
//            codeblock |> toExpr
//            statement |> toExpr
            
        // json := 'sometext';
            
            
        | FSharpExprPatterns.ValueSet(valToSet, valueExpr) ->
            let result = ALExpressionTranslation.translateValueSet b exp
            result 
           
            

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
        
        | FSharpExprPatterns.WhileLoop(guardExpr, bodyExpr, debug1) ->
            let t1 = ()
            let fsalGuard = guardExpr |> ALExpression.ofFSharpExpr b
            let fsalBody = ofFSharpExpr b bodyExpr
            let sequentials =
                match bodyExpr,fsalBody with
                | FSharpExprPatterns.Sequential(curr,next),_ ->
                    let grouped = FSExpr.groupSequentials curr next |> List.map (ofFSharpExpr b >> toStatement)
                    let statements = WhileLoop(fsalGuard,Block(grouped) |> toExpr)
                    statements |> StatementExpr |> FSALExpr
                | _,FSALExpr (StatementExpr (Sequence(alStatement, statement))) ->
                    let block = Block[alStatement;statement]
                    let statements = WhileLoop(fsalGuard,block |> toExpr)
                    statements |> StatementExpr |> FSALExpr
                | _ ->
                    let statements = WhileLoop(fsalGuard,ofFSharpExpr b bodyExpr)
                    statements |> StatementExpr |> FSALExpr
//                    bodyExpr |> ALExpression.ofFSharpExpr b
//                    |> 
            sequentials 
        | FSharpExprPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp, debug1, debug2) ->
            let start = startExpr |> ALExpression.ofFSharpExpr b
            let limit = limitExpr |> ALExpression.ofFSharpExpr b
            let consume = consumeExpr |> ALExpression.ofFSharpExpr b
            let statement = 
                ForLoop(
                    start,
                    limit,
                    consume,
                    isUp
                )
            statement |> StatementExpr |> FSALExpr
        | FSharpExprPatterns.TryFinally(bodyExpr, finalizeExpr, debug1, debug2) ->
            let al_body = bodyExpr |> ALExpression.ofFSharpExpr b |> ALStatement.ofExpression 
            let al_finalize = finalizeExpr |> ALExpression.ofFSharpExpr b |> ALStatement.ofExpression
            Sequence(al_body,al_finalize)
            |> toExpr
            
        | x ->
            failwithf $"expression not implemented %A{x}"
        

    
   
   

    
        
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