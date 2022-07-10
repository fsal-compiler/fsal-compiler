module rec Fs.AL.Compiler.IntermediateLanguage.ALExpressions

open System
open System.Diagnostics
open Fs.AL.Compiler
open Fs.AL.Compiler.CompilerServices
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.IntermediateLanguage.ALContext
open Fs.AL.Compiler.Visitors
open Fs.AL.Core.ALCoreValues
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Compiler.Fullname
open ALLanguage
open Fs.AL.Core.Abstract
open System.Reflection
        
        
module FSharpOperators =
    let (|HasFSharpOperator|_|) b argExprs (s:FSharpSymbol) : ALExpression option =
        let fullname = s.FullName
        let binaryOp operator = Some (handleGenericOperator b argExprs operator)
        match fullname with
        | Operators.op_PipeRight -> Some (handleOperatorPipeRight b argExprs)    
        | Operators.not -> Some (handleOperatorNot b argExprs)    
        | Operators.failwith -> Some (handleOperatorFailwith b argExprs)    
        | Operators.op_Subtract -> binaryOp Subtract 
        | Operators.op_Add -> binaryOp Add 
        | Operators.op_NotEquals -> binaryOp NotEquals   
        | Operators.op_Equals -> binaryOp Equals  
        | Operators.op_GreaterThan -> binaryOp GreaterThan    
        | Operators.op_LessThan -> binaryOp LessThan    
        | Operators.op_Modulo -> binaryOp Mod    
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
        
        
    let private handleOperatorFailwith b argExprs =
        (List.map (ALExpression.ofFSharpExpr b) argExprs)
        |> ALExpression.createInvocation "Error" 


module FSALExpression =
    
    let translateJsonTokenBindExpr (b:ALProcedureContext) (innerLetBinding:FSharpExpr) =
        let outerLetExpr = ALExprContext.getLetBindingCtx b.expressionContext
        
        let selectTokenStr = "SelectToken"
        
        match innerLetBinding, outerLetExpr with 
        | FSharpExprPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo), Some (outermfv,expkind) ->
            
            let jsonroot =
                match objExprOpt.Value with
                | FSharpExprPatterns.Value(valueToGet) ->
                    valueToGet.DisplayName
                | _ -> raise (NotImplementedException())
            
            let fieldname = fieldInfo.Name
            
            // declare compiler-generated var
            let genJsonToken = ALVariable.createGenJsonToken jsonroot fieldname b.localVariables
            genJsonToken |> b.localVariables.Add
            
            //
            // select token to intermediary var
            let selectToken =
                ALExpression.createMemberAccessInvocation (Identifier jsonroot) selectTokenStr ([Constant fieldname ; Identifier genJsonToken.name])
                |> Expression
                
            // declare target var
//            let genField = ALVariable.createGenField jsonroot outermfv.DisplayName (ALType.ofFSharpType outermfv.FullType) b.localVariables
            let genField = ALVariable.createForMfv outermfv
            genField |> b.localVariables.Add
            
            let todo = 1
            let assignmentExpr =
                ALStatement.toBlock
                    [
                        selectToken
                        Assignment(
                            ALExpression.createIdentifer outermfv,
                            JsonTokens.getJsonCastExpr (Identifier genJsonToken.name) outermfv.FullType
                        )
                    ]
//            let final = Sequence (assignmentExpr,Identifier genJsonToken.name |> ALStatement.ofExpression) |> ALExpression.fromALStatement
            LetExprKind.Override,assignmentExpr |> ALExpression.fromALStatement
        | _ ->
            LetExprKind.Normal,innerLetBinding |> ALExpression.ofFSharpExpr b 
    
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
            match chosenDecision with
            | FSALExpr (StatementExpr statement) ->
                let prec,last = ALStatement.unwrapStatement [] statement
                match prec with
                | [] -> last |> ALExpression.fromALStatement
                | prec -> Block (prec @ [ last ] ) |> ALExpression.fromALStatement
            | _ ->
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
                | FSharpExprPatterns.NewRecord _ ->
                    let t = 1
                    ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,LetExprKind.NewRecord)) b.expressionContext
                        (fun f -> LetExprKind.Override,letBindingExpr |> ALExpression.ofFSharpExpr b
                    )
                // empty variable declaration
                | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) ->
                    match argExprs with
                    | [  ] when objType.IsConstructor -> LetExprKind.Declaration,letBindingExpr |> ALExpression.ofFSharpExpr b
                    | [ x ] when objType.IsConstructor && x.Type |> FSharpType.isUnit -> LetExprKind.Declaration,letBindingExpr |> ALExpression.ofFSharpExpr b
                    | _ -> failwith ""
                // constructor assignments
                | FSharpExprPatterns.Let((bindingVarInner, _, _), _) -> 
                    let newIdentifier = "." + letBindingVar.DisplayName
                    ALExprContext.usingCtx (ALExprContext.Constructor newIdentifier) b.expressionContext 
                        (fun f ->
                        LetExprKind.Constructor,letBindingExpr |> ALExpression.ofFSharpExpr b 
                    )
                // decision tree expr
                | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) ->
                    let ekind = LetExprKind.Decision
                    let r = 
                        ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,ekind)) b.expressionContext 
                            (fun f ->
                            let result = letBindingExpr |> ALExpression.ofFSharpExpr b
                            result
                        )
                    let v = 5
                    //letBindingExpr |> ALExpression.ofFSharpExpr b
                    LetExprKind.Decision, r
                // jsonprovider casts
                | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
                    match inpExpr with
                    | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
                        match memberOrFunc.FullName with
                        | "Fable.JsonProvider.jsonParse" ->
                            let expkind = LetExprKind.TypeProvider (memberOrFunc,targetType,argExprs)
                            // explicitly declare variable and override
                            let varToAdd = {
                                isMutable = false
                                name = letBindingVar.DisplayName
                                altype = ALType.ofFSharpType letBindingVar.FullType
                            }
                            varToAdd |> b.localVariables.Add
                            
                            LetExprKind.Override ,
                            ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,expkind)) b.expressionContext (fun f ->
                                letBindingExpr |> ALExpression.ofFSharpExpr b
                            )
                        | "Fable.JsonProvider.getProp" ->
                            let jsonprovtest = 11111
                            let expkind = LetExprKind.TypeProvider (memberOrFunc,targetType,argExprs)
                            ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,expkind)) b.expressionContext (fun f ->
                                LetExprKind.TypeProvider (memberOrFunc,targetType,argExprs),
                                letBindingExpr |> ALExpression.ofFSharpExpr b
                            )
                            
                        | _ -> failwith "unimplemented coerce"
                    | _ -> failwith "unimplemented coerce"
                // TODO: casting
                | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
                    let symbol = memberOrFunc :> FSharpSymbol
                    match symbol with
                    | FSharpSymbol.IsTypeCast ->
                        let v1 = 1
                        let expkind = LetExprKind.TypeProvider (memberOrFunc,typeArgs2[0],argExprs)
//                        let result = ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,expkind)) b.expressionContext (fun f ->
                        let exp =  argExprs[0] |> ALExpression.ofFSharpExpr b
                        let castFrom = typeArgs2[0] |> FSharpType.getRootType
                        let castTo = letBindingVar.FullType |> FSharpType.getRootType
                        let casted = ALExpression.handleTypeCast b castFrom castTo exp letBindingVar
//                        casted
//                        )
                        let t2 = 1
                        // explicitly declare var
                        let varToAdd = {
                            isMutable = false
                            name = letBindingVar.DisplayName
                            altype = ALType.ofFSharpType letBindingVar.FullType
                        }
                        varToAdd |> b.localVariables.Add
                        LetExprKind.Override ,casted |> ALExpression.fromALStatement
                    | _ -> 
                    
                    match symbol.FullName with
                    | Core.GetArray ->
                        let expkind = LetExprKind.TypeProvider (memberOrFunc,typeArgs2[0],argExprs)
                        let xprt = 
                            ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,expkind)) b.expressionContext (fun f ->
//                                LetExprKind.TypeProvider (memberOrFunc,typeArgs2[0],argExprs),
                                letBindingExpr |> ALExpression.ofFSharpExpr b
                            )
                        // explicitly declare var
                        let varToAdd = {
                            isMutable = false
                            name = letBindingVar.DisplayName
                            altype = ALType.ofFSharpType letBindingVar.FullType
                        }
                        varToAdd |> b.localVariables.Add
                        let v1 = 5
                        LetExprKind.Override, xprt
                    | Core.``JsonSerializer.Deserialize`` ->
                        ALVariable.createForMfv letBindingVar |> b.localVariables.Add
                        let expkind = LetExprKind.TypeProvider (memberOrFunc,typeArgs2[0],argExprs)
                        let xprt = 
                            ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,expkind)) b.expressionContext (fun f ->
                                letBindingExpr |> ALExpression.ofFSharpExpr b
                            )
                        match xprt with
                        | FSALExpr (InvocationWithoutTarget (methodname,args)) ->
                            let idf = ALExpression.createIdentifer letBindingVar
                            expkind, ALExpression.createMemberAccessInvocation idf methodname args
                        | _ -> raise (NotImplementedException()) 
                        
                    | _ ->   
                        let test1 = 1
//                        let (FSharpExprPatterns.Coerce(c) ) = argExprs[0]
                        match symbol with 
                        | FSharpSymbol.IsTypeCast ->
                            match argExprs[0] with
                            | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
                                let alexpr = ALExpression.ofFSharpExpr b inpExpr
                                match alexpr with
                                | FSALExpr (InvocationWithoutLastArg (target,method,args)) ->
                                    match method with
                                    | "SelectToken" ->
                                        let expkind = LetExprKind.TypeProvider (memberOrFunc,targetType,argExprs)
                                        let e1 = 
                                            ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,expkind)) b.expressionContext (fun f ->
                                                LetExprKind.TypeProvider (memberOrFunc,targetType,argExprs),
                                                letBindingExpr |> ALExpression.ofFSharpExpr b
                                            )
                                        let targetJsonProp : string  = args[0] |> ALExpression.getConstValue |> unbox 
                                        let jVarName = $".{target}.{targetJsonProp}Token@{b.localVariables.Count}" 
                                        let jArrayVarName = $".{target}.{targetJsonProp}Array@{b.localVariables.Count}" 
                                        // declare intermediary variables
                                        [
                                            ALVariable.createSimple jVarName JsonToken    
                                            ALVariable.createSimple jArrayVarName JsonArray    
                                        ]
                                        |> List.iter b.localVariables.Add
                                        
                                        e1 
                                          
                                        
                                    | n -> failwith $"unimplemented {n}"
                                | _ ->  
                                    
                                LetExprKind.Normal, alexpr // just ignore the coercion
                            | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, _) ->
                                // al does not need a type cast between int/decimal
                                let exp1 = argExprs[0] |> ALExpression.ofFSharpExpr b 
                                LetExprKind.Override, Assignment(ALExpression.createIdentifer letBindingVar,exp1) |> ALExpression.fromALStatement // just ignore the coercion
                            | x -> failwithf $"unimplemented case %A{x}"
                        | FSharpOperators.HasFSharpOperator b argExprs result ->
                            let v1 = 1
                            LetExprKind.Normal,result
                                                
                        | _ ->
                            // default 
                            LetExprKind.Normal,letBindingExpr |> ALExpression.ofFSharpExpr b
                | FSharpExprPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> 
                    let isJsonField = recordOrClassType.TypeDefinition |> FSharpEntity.hasAttribute<AL.Json>
                    match isJsonField with
                    | true ->
                        ALExprContext.usingCtx (ALExprContext.LetBinding (letBindingVar,LetExprKind.Override)) b.expressionContext (fun f ->
                                FSALExpression.translateJsonTokenBindExpr b letBindingExpr
                            )
                    | false -> LetExprKind.Normal,letBindingExpr |> ALExpression.ofFSharpExpr b   
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
            | LetExprKind.Override,_ ->
//                declareVariable()
                let p = 1
                
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
                
                match ALExprContext.getCtorCtx b.expressionContext with
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
                let result = fsalBodyExpr() |> StatementExpr |> FSALExpr
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
//            Debug.Assert(b.expressionContext.Count > 0)
            let context =
                let containingLet =
                    b.expressionContext
                    |> Seq.choose (fun f ->
                        match f with
                        | ALExprContext.LetBinding(valexpr, bindingExpr) ->
                            valexpr |> Some | _ -> None )
                    |> Seq.tryLast
                let targets =
                    decisionTargets
                    |> List.map (fun (args,expr) ->
                        if args.Length > 0
                            then
                                let debug = 1
                                ()
                        let result =
                            expr |> ALExpression.ofFSharpExpr b
                            
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
                ALExprContext.usingCtx context b.expressionContext
                    (fun f ->
                        let createdExpression = decisionExpr |> ALExpression.ofFSharpExpr b
                        let asd1 = 5
                        createdExpression
                )
            res
            
        | _ -> failwith "invalid expression"
        
    let translatePropertySet b (objexpr:FSharpExpr option) (memberOrFunc:FSharpMemberOrFunctionOrValue) (argExprs:FSharpExpr list) : ALExpression =
        let v = 1
        let prop =
            memberOrFunc.DisplayName
            |> (fun f -> f.Replace("`",""))
            |> (fun f -> if f.StartsWith ("set_") then f[4..] else f)
        let assignTo =
            if ALExpression.isThis objexpr.Value
            then Identifier prop
            else
                let tgt = objexpr.Value |> ALExpression.ofFSharpExpr b
                ALExpression.createMemberAccess tgt prop
            
        let newval = (argExprs[0] |> ALExpression.ofFSharpExpr b)
        match newval with
        | FSALExpr (StatementExpr alStatement) ->
            let v = 1
            let prec,last = alStatement |> ALStatement.unwrapStatement []
            let statementblock =
                Block (prec @[
                    Assignment(assignTo,last |> ALExpression.ofALStatement)
                ])
            statementblock |> ALExpression.fromALStatement 
        | _ ->             
            let stat = Assignment(assignTo,newval)
            stat |> ALExpression.fromALStatement 
        
let (|HasALFunctionReplacement|_|) (b:ALProcedureContext) (s:string) : (ALFunctionReplacementArgs -> ALExpression) option =
    match b.registeredReplacements.TryGetValue(s) with
    | true, v -> Some v.Replacement
    | _ -> None

module ALExpression =
    let handleConst (statements:ResizeArray<ALStatement>) (varDeclarations:ResizeArray<ALVariable>)
        (constValueObj:obj, constType:FSharpType) =
            // ignore units
            if constValueObj = () then ()
            else Exit (Constant constValueObj) |> statements.Add
    
    
    let handleTypeCast (b:ALProcedureContext) (fromType:FSharpType) (toType:FSharpType) (exp:ALExpression) (value:FSharpMemberOrFunctionOrValue) : ALStatement =
        match exp with
        | FSALExpr(StatementExpr statement) ->
            let preceding,last = ALStatement.unwrapStatement [] statement
            let tempIdentifier = ("@"+value.DisplayName+"Temp")
            let assignments =
                match fromType |> FSharpType.getFullName, toType |> FSharpType.getFullName with
                | _,Types.String ->
                    // declare variable
                    {
                        name = tempIdentifier
                        isMutable = false
                        altype = ALType.ofFSharpType toType
                    }
                    |> ALProcedureContext.ensureHasVariable b
                    let rside = ALExpression.createInvocation "format" [last |> ALExpression.ofALStatement]
                    let assigment1 = Assignment(Identifier tempIdentifier, rside)
                    let assignment2 = Assignment(ALExpression.createIdentifer value,Identifier tempIdentifier)
                    Sequence(
                        Block (preceding @ [ assigment1]), // generated statements in block
                        assignment2)
                | Types.Double , Types.Int32 ->
                    let assignment2 = Assignment(ALExpression.createIdentifer value,last |> ALExpression.ofALStatement)
                    Sequence(
                        Block (preceding), // generated statements in block
                        assignment2)
                | n -> failwith $"unimplemented cast {n}" 
            
            assignments 
        | _ -> failwith "unimplemented cast"
        
        
    
    
    let rec ofFSharpExpr (b:ALProcedureContext) (exp:FSharpExpr) =
        
        let toExpr (exp:ALStatement) = exp |> StatementExpr |> FSALExpr 
        let toStatement (exp:ALExpression) = exp |> ALStatement.ofExpression 
        Logger.logDebug $"fsharpexpr: %s{FSExpr.getPatternName exp}"
        
        match exp with
        | FSharpExprPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) ->
            //TODO: proper implementation            
//            raise (NotImplementedException())
            Identifier fieldInfo.Name

        | FSharpExprPatterns.NewRecord(recordType, argExprs) ->
            // Todo : new record
            let assignTo = b.expressionContext |> ALExprContext.getLetBindingCtx
            match assignTo with
            | None -> raise (NotImplementedException())
            | Some (letBindVal,kind) ->
                let identifier = ALExpression.createIdentifer letBindVal
                let assignedValues = argExprs |> Seq.map (ALExpression.ofFSharpExpr b)
                let props = recordType.TypeDefinition.FSharpFields |> Seq.map (fun f -> f.Name) 
                let assignmentBlock =
                    (props,assignedValues) ||> Seq.zip
                    |> Seq.map (fun (prop,value) ->
                        let memaccess = ALExpression.createMemberAccess (identifier) prop
                        Assignment(memaccess,value) )
                    |> Seq.toList
                    |> Block
                let f = 1
                assignmentBlock |> ALExpression.fromALStatement
        | FSharpExprPatterns.AddressOf(lvalueExpr) -> lvalueExpr |> ALExpression.ofFSharpExpr b
        | FSharpExprPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) ->
            let result = FSALExpression.translateDecisionTreeSuccess b exp
            result 
            
        | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) ->
            FSALExpression.translateDecisionTree b exp
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
            let result = FSALExpression.translateLet b exp
            result 
            
        | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
            let sym_memberOrFunc = memberOrFunc :> FSharpSymbol
            if memberOrFunc.ApparentEnclosingEntity.IsFSharpRecord && memberOrFunc.IsImplicitConstructor then
                let t = 1
                failwith "test"
            else
            if memberOrFunc.IsPropertySetterMethod || memberOrFunc.IsImplicitConstructor then
                FSALExpression.translatePropertySet b objExprOpt memberOrFunc argExprs
            else
                
                let replacementargs =
                    { context = b.expressionContext
                      variableDeclarations = b.localVariables
                      ALFunctionReplacementArgs.toAL = ofFSharpExpr b
                      objExpr = objExprOpt
                      funcMember = memberOrFunc
                      argExprs = argExprs
                    }
                    
                // match memberfunctionorvalue
                match memberOrFunc with
                | FSharpOperators.HasFSharpOperator b argExprs result -> result
//                | FSharpSymbol.IsTypeCast ->
//                        let v1 = 1
//                        let exp = argExprs[0] |> ALExpression.ofFSharpExpr b
//                        let castFrom = typeArgs2[0] |> FSharpType.getRootType
//                        let castTo = let.FullType |> FSharpType.getRootType
//                        let casted = ALExpression.handleTypeCast b castFrom castTo exp letBindingVar
//                        LetExprKind.Override ,casted |> ALExpression.fromALStatement
                | _ -> 
                // match member fullname
                match memberOrFunc.FullName with
                | HasALFunctionReplacement b replacementFn ->
                    replacementFn replacementargs
                
                | ALFunctionReplacements.HasReplacementFunction (ofFSharpExpr b) objExprOpt memberOrFunc typeArgs2 argExprs result ->
                    let r = 5
                    let replacement = result (ofFSharpExpr b) objExprOpt memberOrFunc typeArgs2 argExprs
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
                
                | x when objExprOpt.IsSome ->
                    // TODO: handle new union case
                    // default
                
                    let func =
                        // remove get_ prefix
                        if memberOrFunc.IsPropertyGetterMethod then
                            memberOrFunc.DisplayName
                            |> (fun f -> f.Replace("`",""))
                            |> (fun f -> if f.StartsWith ("get_") then f[4..] else f)
                        else memberOrFunc.DisplayName
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
                        match objExprOpt.Value |> ALExpression.Value.isReference, memberOrFunc.IsPropertyGetterMethod, memberOrFunc.IsMemberThisValue with
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
                            Identifier func
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
                    | "Microsoft.FSharp.Core.Operators.raise" -> ALExpression.createInvocation "Message" [Constant "raise-exception"]
                    | "Microsoft.FSharp.Collections.Array.length" ->
                    
                        match argExprs with
                        | [FSharpExprPatterns.Value(v)] ->
                            match ALType.ofFSharpType v.FullType with
                            | Simple JsonArray ->
                                match argExprs with
                                | [ x ] ->
                                    let alexp =
                                        ALExpression.createMemberAccess
                                            (Identifier v.DisplayName)
                                            ("Count")
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
                    | x when memberOrFunc.ApparentEnclosingEntity |> FSharpEntity.tryGetALObjectKind |> Option.isSome
//                          &&
//                            (memberOrFunc.ApparentEnclosingEntity |> FSharpEntity.isALObject
//                          || memberOrFunc.ApparentEnclosingEntity.IsFSharpModule)
                        ->
                    
                        let ent = memberOrFunc.ApparentEnclosingEntity
                        let isSingleInstCodeunit = ent |> FSharpEntity.hasAttribute<AL.Codeunit>
                        let isStaticReference = 
                            if isSingleInstCodeunit
                            then Some (ALType.Complex (Codeunit ent.DisplayName))
                            else
                                let at = Type.GetType(ent.FullName)
                                let typebyctor = ent.MembersFunctionsAndValues[0]
                                let gotType = FSharpEntity.tryGetGenericTypeArg ent
                                let isfstype = gotType |> FSharpType.isInheritedFromALType
                                Some (ALType.ofFSharpType gotType.Value)
                            
                        match isStaticReference with
                        | None -> failwith "could not resolve type" 
                        | Some reftype ->
                            let smodule = ALVariable.createStaticModule ent reftype
                            smodule |> ALProcedureContext.ensureHasVariable b     
                        
                            let expr =
                                ALExpression.createMemberAccessInvocation
                                    (Identifier smodule.name)
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
            let letBindContext = (ALExprContext.getLetBindingCtx b.expressionContext)
            let exp = inpExpr |> ofFSharpExpr b
            match letBindContext,exp with
            | None, FSALExpr (InvocationWithoutLastArg (Identifier reader,methodname,args)) ->
                let targetJsonProp : string  = args[0] |> ALExpression.getConstValue |> unbox 
                let v = 1
                // declare intermediary var
                let genJsonToken = ALVariable.createGenJsonToken reader targetJsonProp b.localVariables
                genJsonToken |> b.localVariables.Add
                // select token to intermediary var
                let selectToken =
                    ALExpression.createMemberAccessInvocation (Identifier reader) methodname (args @ [Identifier genJsonToken.name])
                    |> Expression
                // declare target var
                let genField = ALVariable.createGenField reader targetJsonProp (ALType.ofFSharpType targetType) b.localVariables
                genField |> b.localVariables.Add
                // assign var from intermediary
                let assignmentExpr =
                    Sequence(
                        selectToken,
                        Assignment(
                            Identifier genField.name,
                            JsonTokens.getJsonCastExpr (Identifier genJsonToken.name) targetType
                        )
                    )
                    
                Sequence (assignmentExpr,Identifier genField.name |> ALStatement.ofExpression) |> ALExpression.fromALStatement
            | Some (letBindVar,bindingKind), FSALExpr (InvocationWithoutTarget (methodname,args)) ->
                ALProcedureContext.ensureHasJTokenVariable b
                let statement =
                    match bindingKind with
                    | LetExprKind.TypeProvider(memberOrFunctionOrValue, targetType, exprs) ->
                        let test1 = 1
                        // jsonParse
                        ALExpression.createMemberAccessInvocation (Identifier letBindVar.DisplayName) methodname args
                    | _ -> failwith "invalid binding"
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
                        ALExpression.createMemberAccessInvocation (target) methodname (args @ [Identifier letBindVarJsonName])
                        |> Expression
                    // declare target var
                    {   name = letBindVar.DisplayName
                        isMutable = false
                        altype = ALType.ofFSharpType targetType } //ALType.ofFSharpType targetType
                    |> b.localVariables.Add
                    // assign var from intermediary
                    let assignmentExpr =
                        let genType =
                            targetType
                            |> FSharpType.tryGetGenericTypeArg
                            |> Option.map FSharpType.getRootType
                        
                        match genType with
                        // default assignment
                        | None ->
                            let t = 1 
                            Assignment(
                                Identifier letBindVar.DisplayName,
                                JsonTokens.getJsonCastExpr (Identifier letBindVarJsonName) (letBindVar.FullType)
                            )
                        // jsonArray to list variable
                        | Some genericType -> 
                            match genericType |> FSharpType.getFullName with
                            | Types.String | Types.Int32 | Types.Double ->
                                // declare string list to collect values
                                ALStatement.ForeachLoop ((Identifier "_jtoken"),
                                    ALExpression.createMemberAccessInvocation (Identifier letBindVarJsonName) "AsArray" [],
                                    ALExpression.createMemberAccessInvocation
                                        (Identifier letBindVar.DisplayName)
                                        "Add"
                                        [ ALExpression.createJsonCastExpr "_jtoken" genType.Value ] 
                                )
                            | n ->
                                let genfstype = genericType |> ALType.ofFSharpType
                                let v1 = 1
                                match genfstype with
                                | Simple JsonToken ->
                                    Assignment(
                                        Identifier letBindVar.DisplayName,
                                        JsonTokens.getJsonCastExpr (Identifier letBindVarJsonName) (letBindVar.FullType)
                                    )
                                | _ -> failwith "test"
                            | n -> failwithf $"list of %A{n} not implemented"
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
            let result = FSALExpression.translateValueSet b exp
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
//                | FSharpExprPatterns.Sequential(curr,next),_ ->
//                    let grouped = FSExpr.groupSequentials curr next |> List.map (ofFSharpExpr b >> toStatement)
//                    let statements = WhileLoop(fsalGuard,Block(grouped) |> toExpr)
//                    statements |> StatementExpr |> FSALExpr
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
            raise (NotImplementedException()) // TODO: finish
            let start = startExpr |> ALExpression.ofFSharpExpr b
            let limit = limitExpr |> ALExpression.ofFSharpExpr b
            let variable,consume =
                match consumeExpr with
                | FSharpExprPatterns.Lambda(lambdaVar, bodyExpr) ->
                    let v = 1
                    let newvariable = ALVariable.createForMfv lambdaVar
                    newvariable |> b.localVariables.Add
                    // explicitly declare variable and override
                    let prec,last =
                        bodyExpr
                        |> ALExpression.ofFSharpExpr b
                        |> ALStatement.unwrapExpression []
                    let bloc = Block (prec @ [last |> ALStatement.ofExpression])
                    let statements =
                        [
                            yield! prec
                            last |> ALStatement.ofExpression
                        ]
                    let f = 1
                    Identifier newvariable.name ,bloc
                | _ -> raise (NotImplementedException()) 

                //|> ALExpression.ofFSharpExpr b
           
            let statement = 
                ForLoop(
                    variable,
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