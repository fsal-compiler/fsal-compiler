module rec Fs.AL.Compiler.IntermediateLanguage.ALLanguage

open System.Collections.Generic
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.Reflection
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract

type ALBinaryOperator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Mod
    | AND
    | OR
    | XOR
    | LessThan
    | LessThanOrEqual
    | Equals
    | GreaterThan
    | GreaterThanOrEqual
    | NotEquals
    | MemberAccess //Exp.Exp
    | Range // Exp..Exp
    | Scope // Exp::Exp
    
type ALUnaryOperator =
    | UnaryMinus // - Exp
    | Not // NOT Exp
    | Grouping //(Exp)

type FSALMappingExpr =
    | Ignore
    | StatementExpr of ALStatement
    | DecisionExpr of index:int
    | InvocationWithoutTarget of methodnameAndArgs:(string*ALExpression list)
    | InvocationWithoutLastArg of targetMethodNameAndArgs:(ALExpression*string*ALExpression list)

type ALExpression =
    // these are just for mapping F# code
    | FSALExpr of FSALMappingExpr
    //
    | Constant of obj
    | Identifier of string
    | UnaryExp of op:ALUnaryOperator * right:ALExpression
    | Binary of left:ALExpression * op:ALBinaryOperator * right:ALExpression
    | NaryExpression of expr:ALNaryExpression
    | FSharpLambda of newVariable:ALVariable * ALExpression
    with
        /// use only in non-equivalent f# to al mapping cases 
        static member ofALStatement (statement:ALStatement) =
            match statement with
            | Expression alExpression -> alExpression 
            | _ -> failwith "statement is not an expression"
             
        /// todo: needs refactoring
        static member replaceDecisionExpressions
            (assign: bool)
            (usedDecisions: Dictionary<int,ALExpression>)
            (target: string)
            (mappingFunction: FSharpExpr -> ALExpression)
            (decisions:(FSharpMemberOrFunctionOrValue list * FSharpExpr) list)
            (decisionExpr:ALExpression) =
            
            let replace exp =
                exp |> ALExpression.replaceDecisionExpressions assign usedDecisions target mappingFunction decisions
            
            match decisionExpr with
            | FSALExpr(DecisionExpr index) ->
                match usedDecisions.TryGetValue(index) with
                | false,_ ->  
                    let args,expression = decisions[index]
                    let alexp =
                        match assign with
                        | true -> 
                            Assignment (Identifier target,mappingFunction expression)
                            |> StatementExpr
                            |> FSALExpr
                        | false ->
                            let d = 5
                            mappingFunction expression
                    usedDecisions.Add(index,alexp)
                    alexp
                | true,value -> value
            | FSALExpr (StatementExpr alStatement) ->
                ALStatement.replaceDecisionExpressions assign usedDecisions target mappingFunction decisions alStatement
                |> StatementExpr
                |> FSALExpr
            | UnaryExp(alUnaryOperator, alExpression) ->
                UnaryExp(alUnaryOperator,replace alExpression)
            | Binary(alExpression, alBinaryOperator, expression) -> 
                Binary(replace alExpression, alBinaryOperator, replace expression)
            | NaryExpression alNaryExpression ->
                match alNaryExpression with
                | Invocation(alExpression, alExpressions) ->
                    Invocation(replace alExpression, alExpressions |> List.map replace ) |> NaryExpression
                | ArrayReference(alExpression, alExpressions) ->
                    ArrayReference(replace alExpression, alExpressions |> List.map replace ) |> NaryExpression
                | MembershipTest(alExpression, alExpressions) ->
                    MembershipTest(replace alExpression, alExpressions |> List.map replace ) |> NaryExpression
            | _ -> decisionExpr
            
            


type ALNaryExpression =
    | Invocation of target:ALExpression * args:ALExpression list
    | ArrayReference  of target:ALExpression * args:ALExpression list
    | MembershipTest of target:ALExpression * args:ALExpression list

// HVITVED
// 2.4.1 Interface
// Figure 15 The semantic model of a NAV codeunit

// Page 28 BNF grammar for statements
type ALStatement =
    | BeginEnd of ALStatement
    | Sequence of ALStatement*ALStatement
    | Assignment of target:ALExpression * source:ALExpression
    | Expression of exp:ALExpression
    | IfStatement of guard:ALExpression * ``then``:ALStatement * ``else``:ALStatement option   
    | Exit of value:ALExpression
    with
    static member ofExpression (exp:ALExpression) =
        match exp with
        | FSALExpr (StatementExpr alStatement)  -> alStatement
        | _ -> Expression exp
    
    /// todo: needs refactoring    
    static member replaceDecisionExpressions
        (assign: bool)
        (usedDecisions: Dictionary<int,ALExpression>)
        (target: string)
        (mappingFunction: FSharpExpr -> ALExpression)
        (decisions:(FSharpMemberOrFunctionOrValue list * FSharpExpr) list)
        (statement:ALStatement) =
        
        let replaceStatement exp =
            exp |> ALStatement.replaceDecisionExpressions assign usedDecisions target mappingFunction decisions
            
        let replaceExp exp =
            exp |> ALExpression.replaceDecisionExpressions assign usedDecisions target mappingFunction decisions
            
        match statement with
        | BeginEnd alStatement ->
            BeginEnd (replaceStatement alStatement)
        | Sequence(alStatement, statement) ->
            Sequence(replaceStatement alStatement, replaceStatement statement)
        | Assignment(alExpression, expression) ->
            Assignment(replaceExp alExpression, replaceExp expression)
        | Expression exp ->
            Expression ( replaceExp exp)
        | IfStatement(alExpression, alStatement, alStatementOption) ->
            IfStatement(replaceExp alExpression, replaceStatement alStatement,
                alStatementOption |> Option.map replaceStatement)
        | Exit alExpression ->
            Exit (replaceExp alExpression)
            
        
    
type ALComplexType =
    | TODO
    
    

type LetExpressionKind =
    | ExplicitVariableDeclaration of isref:bool * varname:string * vartype:ALType
    | VariableAssignment of varname:string * vartype:ALType * statement:ALStatement
    | InsertLetStatement of statement:ALStatement * nextExpr:LetExpressionKind
    | ALStatement of ALStatement





        
type ALVariable = {isMutable: bool; name:string ; altype:ALType}


module ALExpression =
    
    let createBinary targetExpr operator func  =
        Binary (targetExpr,operator,func)
    
    let createMemberAccess targetExpr func  =
        Binary (targetExpr,ALBinaryOperator.MemberAccess,func)
    
    /// e.g. (Identifier <targetName>).<memberName>(args)
    let createMemberAccessInvocation targetExpr memberName args =
        NaryExpression (Invocation( Binary (
            targetExpr,ALBinaryOperator.MemberAccess,Identifier memberName) ,args))
        
    /// e.g. <methodName>(args)
    let createInvocation methodName args =
        NaryExpression (Invocation(
            (Identifier methodName),args))
        
        
    module Invocation =
        let addArgument (arg:ALExpression) (invocExpr:ALExpression) =
            match invocExpr with
            | NaryExpression alNaryExpression ->
                match alNaryExpression with
                | Invocation(alExpression, alExpressions) ->
                    Invocation(alExpression,alExpressions @ [arg])
                    |> NaryExpression
                | _ -> failwith "unimplemented case"
            | _ -> failwith "unimplemented case"
                
        let changeArgument (arg:ALExpression) (invocExpr:ALExpression) =
            match invocExpr with
            | NaryExpression alNaryExpression ->
                match alNaryExpression with
                | Invocation(alExpression, alExpressions) ->
                    Invocation(alExpression,[arg])
                    |> NaryExpression
                | _ -> failwith "unimplemented case"
            | _ -> failwith "unimplemented case"
                