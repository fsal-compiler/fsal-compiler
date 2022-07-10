module rec Fs.AL.Compiler.IntermediateLanguage.ALLanguage

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Linq
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.Fullname
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
        static member fromALStatement (statement:ALStatement) =
            match statement with
            | Sequence _ -> statement |> StatementExpr |> FSALExpr
            | Assignment _ -> statement |> StatementExpr |> FSALExpr
            | Block _ -> statement |> StatementExpr |> FSALExpr
            | _ -> failwith "not a valid AL statement"
            
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
    | Block of ALStatement list
    | Sequence of ALStatement*ALStatement
    | Assignment of target:ALExpression * source:ALExpression
    | Expression of exp:ALExpression
    | IfStatement of guard:ALExpression * ``then``:ALStatement * ``else``:ALStatement option
    // TODO : case, while, repeat, for, with(?), foreach 
    | ForLoop of lambdaIdentifier:ALExpression * assignment:ALExpression * exp:ALExpression * doStatement:ALStatement * isUp:bool  
    | ForeachLoop of identifier:ALExpression * exp:ALExpression * doStatement:ALExpression
    | WhileLoop of guard:ALExpression * doStatement:ALExpression   
    | Exit of value:ALExpression
    with
    
    /// replaces Assignment statement identifier
    static member withAssignmentIdentifier (newIdentifier:string) assignmentStatement =
        let dasdasd = 1
        match assignmentStatement with
        | Assignment(Binary(tgtId, MemberAccess, memberId), bindVal) ->
            Assignment(Binary(Identifier newIdentifier, MemberAccess, memberId), bindVal)
        | Assignment (tgt, bindVal) ->
            Assignment(Identifier newIdentifier, bindVal)
        | _ -> failwith "not a valid AL assigment expr"
        
    static member withoutNullStatements statements =
        statements |> List.where (fun f ->
            match f with
            | ALStatement.Expression (Constant null) -> false 
            | _ -> true
        )
        
    static member sequenceOf (exprs:ALStatement list) =
        match exprs with
        | e1::e2::tail ->
            tail
            |> List.fold
                (fun acc f ->
                    match acc with 
                    | Sequence(e1',e2') -> 
                        Sequence(
                            e1',
                            Sequence(e2',f)
                        )
                    | _ -> failwith "invalid sequence"
                )
                (Sequence(e1,e2))
        | _ -> failwith "invalid sequence"  
         
        
        
    static member withAssignmentTarget (targetExp:ALExpression) assignmentStatement =
        match assignmentStatement with
        | Assignment (tgt, assignment) ->
            Assignment(tgt, targetExp)
        | _ -> failwith "not a valid AL assigment expr"
        
    /// unwraps block statements
    static member toStatements assignmentStatement =
        match assignmentStatement with
        | Block (statements) -> statements
        | _ -> failwith "not a valid block statement"
        
    static member getLastStatement (statements:'t list) =
        statements[..statements.Length - 2],statements[statements.Length - 1]
        
    static member toBlock statements = Block statements
    
    static member unwrapStatement acc (stat:ALStatement) =
        match stat with
        | Sequence(currSt, nextSt) ->
            let precLeft,lastLeft = ALStatement.collectSequencesLast (currSt) 
            let precRight,lastRight = ALStatement.collectSequencesLast (nextSt)
            (precLeft@precRight@[lastLeft])@ acc,lastRight
        | Expression alExpression ->
            acc, Expression alExpression
        | st -> acc, st
    static member unwrapExpression acc (stat:ALExpression) : ALStatement list * ALExpression =
        match stat with
        | Binary(left, alBinaryOperator, right) ->
            let precLeft,lastLeft = ALStatement.unwrapExpression [] (left) 
            let precRight,lastRight = ALStatement.unwrapExpression [] (right)
            (precLeft@precRight)@ acc ,Binary(lastLeft, alBinaryOperator, lastRight)
        | FSALExpr (StatementExpr alStatement) ->
            let prec,remainingStatement = ALStatement.unwrapStatement [] alStatement
            match remainingStatement with
            | Sequence(alStatement, statement) ->
                prec @ [alStatement], statement |> StatementExpr |> FSALExpr
            | _ -> prec, remainingStatement |> StatementExpr |> FSALExpr
        | other -> acc ,other 
    
    static member collectSequences acc (stat:ALStatement) =
        match stat with
        | Sequence(alStatement, statement) ->
            let left = ALStatement.collectSequences acc alStatement
            ALStatement.collectSequences (left) statement
        | stat ->
            stat :: acc
         
    static member collectSequencesLast (stat:ALStatement) =
        let allstatements = ALStatement.collectSequences [] stat 
        let preceding,last = allstatements.Tail |> List.rev,allstatements.Head
        preceding,last
            
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
        | Block alExprs ->
            let b1 = 1
            Block (alExprs |> List.map replaceStatement )
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
            
        
    static member getExitSequence curr2 (fsalExp:ALStatement) =
        match fsalExp with
        | Sequence(curr, next) ->
             ALStatement.getExitSequence curr (next) 
        | x ->
            match x with
            | Expression (Constant o) -> // change last statement to exit condition
                Sequence(curr2,(Exit(Constant o)))
            | Expression (NaryExpression (Invocation(alExpression, alExpressions))) ->
                Sequence(curr2,(Exit(NaryExpression (Invocation(alExpression, alExpressions)))))
            | Expression (Binary(alExpression, alBinaryOperator, expression)) ->
                Sequence(curr2,Exit(Binary(alExpression, alBinaryOperator, expression)))
            | Expression (Identifier s) ->
                Sequence(curr2,(Exit(Identifier s)))
            | _ -> failwith "unimplemented exit condition"

    static member withExit last : ALStatement =
        let debug = 5
        let upd =
            match last with
            | Sequence(alStatement, statement) ->
                Sequence(alStatement,ALStatement.withExit statement)
            | Expression (Constant o) -> // change last statement to exit condition
                Exit(Constant o)
            | Expression (NaryExpression (Invocation(alExpression, alExpressions))) ->
                Exit(NaryExpression (Invocation(alExpression, alExpressions)))
            | Expression (Binary(alExpression, alBinaryOperator, expression)) ->
                Exit(Binary(alExpression, alBinaryOperator, expression))
            | Expression (Identifier s) ->
                Exit(Identifier s)
            | IfStatement(guard, thenx, elsex) ->
                IfStatement(
                    guard,
                    thenx |> ALExpression.ofALStatement |> Exit,
                    elsex |> Option.map (ALExpression.ofALStatement >> Exit)
                )
            | Block alStatements ->
                let prec,last = ALStatement.getLastStatement alStatements
                let newBlockStatements =
                    prec @ [Exit(last |> ALExpression.ofALStatement)]
                Block newBlockStatements
            | Exit alExpression ->
                failwith "statement already has exit"
            | _ -> failwith "unimplemented exit condition"
        upd
        
//    static member printAST (fsalExp:ALStatement) =
//        match fsalExp with
//        | Sequence(curr, next) ->
//             ALStatement.getExitSequence curr (next) 
//        | Assignment (tgt, value) ->
//            printfn $"%A{}"
//        | Expression (Constant o) -> // change last statement to exit condition
//            Sequence(curr2,(Exit(Constant o)))
//        | Expression (NaryExpression (Invocation(alExpression, alExpressions))) ->
//            Sequence(curr2,(Exit(NaryExpression (Invocation(alExpression, alExpressions)))))
//        | Expression (Binary(alExpression, alBinaryOperator, expression)) ->
//            Sequence(curr2,Exit(Binary(alExpression, alBinaryOperator, expression)))
//        | Expression (Identifier s) ->
//            Sequence(curr2,(Exit(Identifier s)))
//        | _ -> failwith "unimplemented exit condition"
        
type LetExpressionKind =
    | ExplicitVariableDeclaration of isref:bool * varname:string * vartype:ALType
    | VariableAssignment of varname:string * vartype:ALType * statement:ALStatement
    | InsertLetStatement of statement:ALStatement * nextExpr:LetExpressionKind
    | ALStatement of ALStatement

type ALVariable = {isMutable: bool; name:string ; altype:ALType}

/// replace function by fullname
[<AttributeUsage(AttributeTargets.All,Inherited=true,AllowMultiple=false)>]
type ALReplaceFunctionAttribute(functionFullname:string) = 
    inherit System.Attribute()
    member this.Name : string = ""    
//"Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetArray"



