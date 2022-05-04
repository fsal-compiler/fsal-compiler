module Fs.AL.Compiler.ExpressionReader

open System.Collections.Generic
open System.Collections.ObjectModel
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.IntermediateLanguage.ALContext
open Fs.AL.Compiler.IntermediateLanguage.FSharpExpressionsMapping
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Compiler.Reflection
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract



module Debugging =
    do ()
//    let saveToJson statements =
//        let jsonopts = JsonSerializerOptions()
//        jsonopts.Converters.Add(JsonFSharpConverter())
//        let js = JsonSerializer.Serialize(statements,jsonopts)
//        File.WriteAllText(@"C:\temp\statements.json",js)

    

module ReadProcedureBody =
    let handleConst (statements:ICollection<ALStatement>) (varDeclarations:ICollection<ALVariable>)
        (constValueObj:obj, constType:FSharpType) : unit =
            // ignore units
            if constValueObj = () then ()
            else Exit (Constant constValueObj) |> statements.Add
        
    let rec handleLetExpression (b:ALProcedureContext)
        (letBinding:FSharpMemberOrFunctionOrValue)
        (expression:FSharpExpr) (next:FSharpExpr) : unit =
        
//        Logger.logDebug $"handleLetExpression:{expression} {expression.Type}"
//        Visitors.printast expression
        
        {
            ALVariable.isMutable = false
            name = letBinding.DisplayName
            altype = letBinding.FullType |> ALType.ofFSharpType
        }
        |> b.localVariables.Add
        //
        match next with
        | FSharpExprPatterns.Value(valueToGet) ->
            if letBinding = valueToGet then ()
        | _ ->
            let d = 5
            // todo: handle this case for inner let bindings
            () 
        
        let result = 
            match expression with
            | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) ->
                // variable declaration
                match argExprs with
                | [ x ] when x.Type |> FSharpType.isUnit -> ()
                | _ ->
                    let debug = 5
                    ()
                
            | FSharpExprPatterns.Const(constValueObj, constType) ->
                
                // number value declaration
                let statement = (Assignment (Identifier letBinding.DisplayName,Constant constValueObj))
                // add variable
//                {
//                    ALVariable.isMutable = false
//                    name = letBinding.DisplayName
//                    altype = constType |> ALType.ofFSharpType
//                }
//                |> b.localVariables.Add
                // assign variable
                b.statements.Add(statement)
            | FSharpExprPatterns.Value(valueToGet) ->
                let statement = (Assignment (Identifier letBinding.DisplayName,Identifier valueToGet.DisplayName))
                b.statements.Add(statement)
            | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
                // jsonprovider parsing functions
                // todo: needs refactoring
                let a = 5
                if true then
                    let test = true
                    let exp = inpExpr |> ALExpression.ofFSharpExpr b
                    // assign a from .ReadFrom (json)
                    let target = Identifier letBinding.DisplayName
                    match exp with
                    | FSALExpr (InvocationWithoutTarget (methodname,args)) ->
                        ALExpression.createMemberAccessInvocation target methodname args
                        |> Expression
                        |> b.statements.Add
                    | FSALExpr (InvocationWithoutLastArg (tgt,methodname,args)) ->
                        //todo:json-parsing:selecttoken
                        JsonTokens.handleSelectToken b letBinding tgt methodname args 
                        
//                        Assignment(target)
                        let ff = 5
                        ()
                        
                        
                    | NaryExpression alNaryExpression ->
                        // let <target> = <expression>
                        Assignment (target,exp)
                        |> b.statements.Add
                    | Binary(alExpression, alBinaryOperator, expression) ->
                        // let <target> = <expression>
                        Assignment (target,exp)
                        |> b.statements.Add
                    | _ ->
                        failwith "unhandled case "
                   
//                    Assignment(Identifier binding.DisplayName, exp) |> b.statements.Add
                    let d = 5 
                    ()
                else
                // handle typeprovider exprs
                match inpExpr with
                | FSharpExprPatterns.Let((innerbinding, innervalue, e), next) ->
                    let a = 5
                    //handleLetExpression b binding' value' next
                    // parse json
                    let exp =
                        ALExpression.createMemberAccessInvocation
                            (Identifier letBinding.DisplayName)
                            "ReadFrom"
                            [innervalue |> ALExpression.ofFSharpExpr b]
                    let v1 = 5
                    // binding for parsed json
//                    {
//                        ALVariable.isMutable = false
//                        name = innerbinding.DisplayName
//                        altype = Simple (SimpleType "JsonToken")
//                    }
//                    |> b.localVariables.Add
                    // parse to variable instead
//                    let newexp = exp |> ALExpression.Invocation.addArgument (Identifier innerbinding.DisplayName)
                    b.statements.Add(Expression exp)
                    
                | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
                    let assignmenttype = Simple (SimpleType "JsonToken")
                    // add variable
                    {
                        ALVariable.isMutable = false
                        name = letBinding.DisplayName
                        altype = Simple (SimpleType "JsonToken")
                    }
                    |> b.localVariables.Add
                    
                    let e1 = argExprs[0] |> ALExpression.ofFSharpExpr b
                    let e2 = argExprs[1] |> ALExpression.ofFSharpExpr b
                    let expr = Assignment(e1,e2)
                    // assign variable
//                    let ex = argExprs[0] |> ALExpression.ofFSharpExpr b
                    b.statements.Add(expr)
                    ()
                | _ ->
                    let argad = 5
                    
                    //let exp = ALExpression.ofFSharpExpr b inpExpr |> ALExpression.Invocation.addArgument (Identifier binding.DisplayName)
                    let assignmenttype = Simple (SimpleType "JsonToken")
                    // add variable
//                    {
//                        ALVariable.isMutable = false
//                        name = binding.DisplayName
//                        altype = Simple (SimpleType "JsonToken")
//                    }
//                    |> b.localVariables.Add
                    // assign variable
//                    let ex = innervalue |> ALExpression.ofFSharpExpr b
//                    b.statements.Add(Expression next)
                    ()
                
            | FSharpExprPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) ->
                // deserializing json
                
                let isjson = recordOrClassType.TypeDefinition |> FSharpEntity.hasAttribute<ALJson>
                if isjson then
                    //JContent.SelectToken('details',JDetails);
                    let realstatement =()
                    ()
                let debug = 5
                b.statements.Add(Exit (Identifier ""))
                
            | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs)
                
                // mutable variable declaration
                when memberOrFunc |> FSharpMemberOrFunctionOrValue.isRefOperator ->
                let altype = letBinding.FullType |> ALType.ofFSharpType
                match argExprs[0] with
                | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) ->
                    ()
                | FSharpExprPatterns.Const(constValueObj, constType) ->
                    ()
                | _ ->  failwith "unimplemented case"
                
            | FSharpExprPatterns.Call(x) ->
                // details.locations[0]
                // add variable
                let d = 5
                // handle json array here
                
                
                // add statement
                let newvalue = ALExpression.ofFSharpExpr b expression
                match newvalue with
                | FSALExpr (InvocationWithoutLastArg (target,method,args)) ->
                    JsonTokens.handleSelectToken b letBinding target method args
                | _ -> 
                    let statement = (Assignment (Identifier letBinding.DisplayName,newvalue))
                    b.statements.Add statement
                
            | FSharpExprPatterns.TypeLambda(genericParam, bodyExpr) -> // todo: try with expr 
                let a = 5
                let invoc = bodyExpr |> ALExpression.ofFSharpExpr b
//                    ALExpression.createMemberAccessInvocation
//                        (bodyExpr |> ALExpression.ofFSharpExpr b)
//                        "SelectToken"
//                        (Identifier "asdasd" |> List.singleton)
    //                let statement = (Assignment (Identifier assignedto,invoc))
                
                // add variable
                {
                    ALVariable.isMutable = true
                    name = letBinding.DisplayName
                    altype = letBinding.FullType |> ALType.ofFSharpType
                }
                |> b.localVariables.Add
                // add statement
                b.statements.Add (Expression invoc)
                
            | FSharpExprPatterns.Let((innerbinding, innerexpression, e), innerNext) ->
                
                // let inside let
                // outer - record
                // inner - returnval
                let stat = Expression(ALExpression.ofFSharpExpr b innerNext)
                // declare inner let
                {
                    ALVariable.isMutable = false
                    name = innerbinding.DisplayName
                    altype = innerbinding.FullType |> ALType.ofFSharpType
                }
                |> b.localVariables.Add
                match stat with
                | Expression (Identifier returnVal) ->
                    // assign outer let with return value
                    let statement =
                        Assignment
                            (Identifier letBinding.DisplayName, Identifier returnVal)
                    b.statements.Add statement
                | _ ->
                    failwithf $"%A{stat}"
                    b.statements.Add stat
                    ()
    //          
            | FSharpExprPatterns.Lambda(lambdaVar, bodyExpr) -> 
                // this does not work
                let stat = 
                    match bodyExpr with
                    | FSharpExprPatterns.Let((bindingVar, bindingExpr, debug1), exprBody) ->
                        {
                            ALVariable.isMutable = false
                            name = bindingVar.DisplayName
                            altype = bindingVar.FullType |> ALType.ofFSharpType
                        }
                        |> b.localVariables.Add
                        
                        // TODO: CTOR
//                        let newval = bindingExpr |> ALExpression.ofFSharpExpr b
                        match bindingExpr with
                        | FSharpExprPatterns.Const(constValueObj, constType) ->
                            if constValueObj = () then ()
                            else failwithf $"%A{constValueObj}"
                        | _ -> ()
//                        let assignstatement = Assignment(Identifier bindingVar.DisplayName , bindingExpr |> ALExpression.ofFSharpExpr b)
//                        b.statements.Add assignstatement
                        let nextstatement = exprBody |> ALExpression.ofFSharpExpr b
                        Expression nextstatement
                        
                    | _ ->  Expression(ALExpression.ofFSharpExpr b bodyExpr)
                    
                // declare inner let
                {
                    ALVariable.isMutable = false
                    name = lambdaVar.DisplayName
                    altype = lambdaVar.FullType |> ALType.ofFSharpType
                }
                |> b.localVariables.Add
                let d = 5
                match stat with
                | Expression (Identifier returnVal) ->
                    // assign outer let with return value
                    let statement =
                        Assignment
                            (Identifier letBinding.DisplayName, Identifier returnVal)
                    b.statements.Add statement
                | Expression (FSALExpr Ignore) -> ()
                | _ ->
                    failwithf $"%A{stat}"
                    b.statements.Add stat
            | FSharpExprPatterns.Application(funcExpr, typeArgs, argExprs) ->
                // inner functions
                let exprs =
                    argExprs |> List.map (ALExpression.ofFSharpExpr b)
                let func = funcExpr |> ALExpression.ofFSharpExpr b
                let d = 5
//                ALExpression.createInvocation func exprs
                let a = 5
                ()
            | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
                // assign if else is different in AL
                
                let guardStatement = guardExpr |> ALExpression.ofFSharpExpr b
                let thenStatement = thenExpr |> ALExpression.ofFSharpExpr b |> Expression
                let elseStatementOpt = 
                    match elseExpr |> ALExpression.ofFSharpExpr b with
                    | Constant null -> None
                    | elseStatement -> Some (Expression elseStatement)
                
                match elseStatementOpt with
                | None ->
                    let ifstatement =
                        IfStatement(
                            guardStatement,
                            Assignment((Identifier letBinding.DisplayName),
                                ALExpression.ofALStatement thenStatement),
                            None
                        )
                    ifstatement |> b.statements.Add
                    ()
                | Some elseStatement -> 
                    let ifstatement =
                        IfStatement(
                            guardStatement,
                            Assignment((Identifier letBinding.DisplayName),
                                ALExpression.ofALStatement thenStatement),
                            Assignment((Identifier letBinding.DisplayName),
                                ALExpression.ofALStatement elseStatement)
                            |> Some 
                        )
                    ifstatement |> b.statements.Add
            | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) ->
                // pattern match case is different in AL
                let createdExpression = decisionExpr |> ALExpression.ofFSharpExpr b

                // todo: this may add unnecessary variables
                // figure out a way to evaluate once
//                let bcopy =
//                    { b with
//                        localVariables = ObservableCollection()
//                        statements = ObservableCollection() }
//                                
                // assign decision targets
                let expressionWithDecisions =
                    createdExpression
                    |> ALExpression.replaceDecisionExpressions
                           true
                           (System.Collections.Generic.Dictionary())
                           letBinding.DisplayName
                           (ALExpression.ofFSharpExpr b)
                           decisionTargets
                let d = 5
                // todo: replace decision targets
                ALStatement.ofExpression expressionWithDecisions
                |> b.statements.Add
                
//                failwith $"decision trees are not yet implemented: %A{decisionExpr}"
            | x ->
                let a = 5
                failwithf $"%A{x}"
        let debug = 5
//        result
        ()
        

    

let readProcedureBody (b:ALProcedureContext) body =
    
    body
    |> Visitors.visitFunctionBody (fun e ->
        if b.entity.DisplayName = "SharedRecordType" then
            let debug = 5
            ()
//            Logger.logDebug $"readProcedureBody:"
//            Visitors.printast e
//        
        match e with
        | FSharpExprPatterns.Let((binding, value, e), next) ->
            ReadProcedureBody.handleLetExpression b binding value next
        | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
//            if memberOrFunc.IsPropertyGetterMethod && (not e.ImmediateSubExpressions.IsEmpty) then () else
            // TODO: jsonprovider
            let expr = ALExpression.ofFSharpExpr b e
            b.statements.Add(Expression expr)
        | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
            let guardStatement = guardExpr |> ALExpression.ofFSharpExpr b
            let thenStatement = thenExpr |> ALExpression.ofFSharpExpr b |> Expression
            let elseStatement = 
                match elseExpr |> ALExpression.ofFSharpExpr b with
                | Constant null -> None
                | elseStatement -> Some (Expression elseStatement)
            IfStatement( guardStatement,thenStatement,elseStatement )
            |> b.statements.Add
            
        | FSharpExprPatterns.Const x ->
            let debug = 5
            ReadProcedureBody.handleConst b.statements b.localVariables x
            // return simple const value form fn
        | x ->
            let expt = x.Type
            let debug = 5
            match x with
            | FSharpExprPatterns.Value(valueToGet) -> // return value
                Exit(Identifier valueToGet.DisplayName) |> b.statements.Add 
            | y -> () // from let expr
            // value val content
            ()
    )
    
    let st = 5
    
    
    
    
//    let st = (b.localVariables|> Seq.toList,b.statements |> Seq.toList)
    
    b
    
