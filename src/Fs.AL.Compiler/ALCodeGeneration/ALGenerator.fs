module rec Fs.AL.Compiler.ALCodeGeneration.ALGenerator

open System.IO
open Fs.AL.Compiler
open Fs.AL.Compiler.ALBuilder
open Fs.AL.Compiler.ALCodeGeneration.Common
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis.Syntax
open Microsoft.FSharp.Linq.RuntimeHelpers
open Nodes
module t = Common.Trivia
type private sf = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxFactory
type private sk = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxKind


let rec buildUnaryExpression (op,a1) : CodeExpressionSyntax =
    match op with
    | Not ->
        let innerExp = a1 |> buildExpression :?> CodeExpressionSyntax
        let notexp =
            sf.UnaryExpression(SyntaxKind.UnaryNotExpression,innerExp)
                .WithOperatorToken(sf.Token(sk.NotKeyword).WithTrailingTrivia(sf.Space))
        notexp
    | Grouping ->
        let innerExp = a1 |> buildExpression :?> CodeExpressionSyntax
        sf.ParenthesizedExpression(innerExp)
    | x -> failwithf $"%A{x}"
    
let rec buildBinaryExpression (a1,op,a2) : CodeExpressionSyntax =
    let exp1 = a1 |> buildExpression :?> CodeExpressionSyntax 
    let exp2 = a2 |> buildExpression :?> CodeExpressionSyntax
    match op with
    | MemberAccess -> sf.MemberAccessExpression(exp1,exp2 :?> IdentifierNameSyntax)
    | _ ->
    match op with
    | Add -> sf.BinaryExpression(sk.AddExpression,exp1,exp2)
    | Subtract -> sf.BinaryExpression(sk.SubtractExpression,exp1,exp2)
    | Multiply -> sf.BinaryExpression(sk.MultiplyExpression,exp1,exp2)
    | Divide -> sf.BinaryExpression(sk.DivideExpression,exp1,exp2)
    | Mod -> sf.BinaryExpression(sk.ModuloExpression,exp1,exp2)
    | Equals -> sf.BinaryExpression(sk.EqualsExpression,exp1,exp2)
    | NotEquals -> sf.BinaryExpression(sk.NotEqualsExpression,exp1,exp2)
    | GreaterThan -> sf.BinaryExpression(sk.GreaterThanExpression,exp1,exp2)
    | GreaterThanOrEqual -> sf.BinaryExpression(sk.GreaterThanOrEqualExpression,exp1,exp2)
    | LessThan -> sf.BinaryExpression(sk.LessThanExpression,exp1,exp2)
    | LessThanOrEqual -> sf.BinaryExpression(sk.LessThanOrEqualExpression,exp1,exp2)
    | OR -> sf.BinaryExpression(sk.LogicalOrExpression,exp1,exp2)
    | AND -> sf.BinaryExpression(sk.LogicalAndExpression,exp1,exp2)
    | XOR -> sf.BinaryExpression(sk.LogicalXorExpression,exp1,exp2)
    | Range -> sf.BinaryExpression(sk.RangeExpression,exp1,exp2)
    | x -> failwithf $"%A{x}"
    |> (fun f -> f.WithOperatorToken(f.OperatorToken.WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space)))

let rec buildNaryExpression (exp:ALNaryExpression) =
    let nar = 5
    match exp with
    | Invocation(alExpression, alExpressions) ->
        let tgt = alExpression |> buildExpression :?> CodeExpressionSyntax
        let args =
            alExpressions
            |> List.map (fun f -> buildExpression f :?> CodeExpressionSyntax)
        let separatedSyntaxList = SeparatedSyntaxList()
        match args with
        | [] -> sf.InvocationExpression(tgt)
        | _ -> 
            let newl = separatedSyntaxList.AddRange(args)
            let nal = sf.ArgumentList(newl)
            sf.InvocationExpression(tgt,nal)
        
    | x -> failwithf $"%A{x}"
    
    

let rec buildExpression (exp:ALExpression) : ExpressionSyntax =
    match exp with
    | NaryExpression naryExp ->
        let alexp = naryExp |> buildNaryExpression
        alexp :> ExpressionSyntax
    | Binary (a1,op,a2) ->
        (a1,op,a2) |> buildBinaryExpression :> ExpressionSyntax
    | Identifier s ->
        sf.IdentifierName(s)
    | Constant o ->
        match o with
        | null -> null
        | :? int as v -> 
            sf.Literal(v)
            |> sf.Int32SignedLiteralValue
            |> sf.LiteralExpression
            :> ExpressionSyntax
        | :? bool as v ->
            let lit =
                if v then sf.BooleanLiteralValue(sf.Token(sk.TrueKeyword))
                else sf.BooleanLiteralValue(sf.Token(sk.FalseKeyword))
            lit
            |> sf.LiteralExpression
            :> ExpressionSyntax
        | :? string as v ->
            sf.Literal(v)
            |> sf.StringLiteralValue
            |> sf.LiteralExpression
            :> ExpressionSyntax
        | _ ->
            sf.Literal(o :?> string)
            |> sf.StringLiteralValue
            |> sf.LiteralExpression
            :> ExpressionSyntax
    | UnaryExp (op,a1) ->
        let alExp = (op,a1) |> buildUnaryExpression
        alExp
    | FSharpLambda(newVariable, alExpression) ->
        buildExpression alExpression
        

    | x -> failwithf $"%A{x}"        


/// map expressions from intermediate language to AL 
module ALStatementImpl =
    let createAssignment ((assignedTo:ALExpression),(expression:ALExpression)) =
        // todo: unwrap assignment trees
        match expression with
        | FSALExpr (StatementExpr (Assignment(origtgt, value))) ->
            let target = assignedTo |> buildExpression :?> CodeExpressionSyntax
            let assignedvalue = value |> buildExpression :?> CodeExpressionSyntax
            let assignStatement =
                sf.AssignmentStatement(target,assignedvalue)
                    .WithAssignmentToken(sf.Token(sk.AssignToken).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
                    .WithSemicolonToken(sf.Token(sk.SemicolonToken))
            assignStatement
        | _ ->  
            let target = assignedTo |> buildExpression :?> CodeExpressionSyntax
            let assignedvalue =
                match expression with
                | _ -> expression |> buildExpression :?> CodeExpressionSyntax
            let assignStatement =
                sf.AssignmentStatement(target,assignedvalue)
                    .WithAssignmentToken(sf.Token(sk.AssignToken).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
                    .WithSemicolonToken(sf.Token(sk.SemicolonToken))
            assignStatement
       
    let createIfElse (guard: ALExpression) (``thenExp`` : ALStatement) (``elseExpOpt`` : ALStatement option) =
        let alguard = (ALUnaryOperator.Grouping,guard) |> buildUnaryExpression
        let althen : StatementSyntax =
            match thenExp,elseExpOpt with
            | Expression (FSALExpr (StatementExpr alStatement)),_ ->
                let st : StatementSyntax list = [alStatement] |> buildStatements []
                match st.[0] with
                | :? AssignmentStatementSyntax as s ->
                    s.WithSemicolonToken(SyntaxToken())
                | _ -> st[0]
            | Expression e, _ ->
                let expr = e |> buildExpression :?> CodeExpressionSyntax
                match elseExpOpt with
                | Some _ ->  sf.ExpressionStatement(expr)
                | None -> sf.ExpressionStatement(expr).WithSemicolonToken(sf.Token(sk.SemicolonToken))
                |> (fun f -> f.WithTrailingTrivia(sf.Space))
            | Assignment(assignedTo, expression),Some _ ->
                ALStatementImpl.createAssignment(assignedTo,expression)
                    .WithSemicolonToken(SyntaxToken())
            | Assignment(assignedTo, expression),None ->
                ALStatementImpl.createAssignment(assignedTo,expression)
            | _ -> failwithf $"unhandled case"
            
        let alelseopt : (StatementSyntax option) = elseExpOpt |> Option.map (fun f ->
            match f with
            | Expression (FSALExpr (StatementExpr alStatement)) ->
                let st = [alStatement] |> buildStatements []
                st[0]
            | Expression e ->
                let exp = e |> buildExpression :?> CodeExpressionSyntax
                let st = exp |> sf.ExpressionStatement
                st.WithSemicolonToken(sf.Token(sk.SemicolonToken))
            | Assignment(assignedTo, expression) ->
                ALStatementImpl.createAssignment(assignedTo,expression)
                    
            | _ -> failwithf $"unhandled case"
        )
        let statement =
            match alelseopt with
            | None ->
                sf.IfStatement(alguard,althen)
                    .WithIfKeywordToken(sf.Token(sk.IfKeyword).WithTrailingTrivia(sf.Space))
                    .WithThenKeywordToken(sf.Token(sk.ThenKeyword).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
                    //                .WithSemicolonToken(sf.Token(sk.SemicolonToken))
            | Some elseStatement ->
                sf.IfStatement(
                    alguard,
                    althen.WithTrailingTrivia(sf.Space),
                    elseStatement)
                    .WithIfKeywordToken(sf.Token(sk.IfKeyword).WithTrailingTrivia(sf.Space))
                    .WithElseKeywordToken(sf.Token(sk.ElseKeyword).WithTrailingTrivia(sf.Space))
                    .WithThenKeywordToken(sf.Token(sk.ThenKeyword).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
        statement

let rec buildStatements acc (statements:ALStatement list) =
    match statements with
    | [] ->
        acc
        |> List.map (fun f -> f.WithLeadingTrivia(Trivia.lf8spaces))
        |> List.rev
    | head::tail ->
        match head with
        | Expression fsalExp ->
            match fsalExp with
            | FSALExpr Ignore ->  buildStatements acc tail
            | FSALExpr (StatementExpr alStatement) ->
//                let al_exp = ALExpression.ofALStatement alStatement
//                let ALexp = buildExpression fsalExp :?> CodeExpressionSyntax
                let buildstat = buildStatements [] [alStatement] |> List.head
//                let statement = sf.ExpressionStatement(ALexp).WithSemicolonToken(sf.Token(sk.SemicolonToken)) :> StatementSyntax
                buildStatements (buildstat::acc) tail
            | NaryExpression alNaryExpression ->
                let ALexp = buildExpression fsalExp :?> CodeExpressionSyntax
                let statement = sf.ExpressionStatement(ALexp).WithSemicolonToken(sf.Token(sk.SemicolonToken)) :> StatementSyntax
                buildStatements (statement::acc) tail
            | Binary (left,op,right) ->
                let ALexp = buildExpression fsalExp :?> CodeExpressionSyntax
                let statement = sf.ExpressionStatement(ALexp).WithSemicolonToken(sf.Token(sk.SemicolonToken)) :> StatementSyntax
                buildStatements (statement::acc) tail
            | _ -> failwith "test"
                
            
        | Assignment(alExpression, FSALExpr (StatementExpr alStatement)) ->
            //todo: unwrap assignment
            match alStatement with
            | IfStatement(guard, thenExp, elseExpOpt) ->
                let statement = 
                    ALStatementImpl.createIfElse
                       guard
                       (Assignment(alExpression,ALExpression.ofALStatement thenExp))
                       (elseExpOpt |> Option.map (fun f -> Assignment(alExpression, f |> ALExpression.ofALStatement)))
                buildStatements (statement::acc) tail
                
            | _ -> failwith "unhandled case"
            
        | Assignment(alExpression, expression) ->
            let statement = ALStatementImpl.createAssignment(alExpression,expression)
            buildStatements (statement::acc) tail
        | IfStatement(guard, thenExp, elseExpOpt) ->
            let alguard =
                (ALUnaryOperator.Grouping,guard)
                |> buildUnaryExpression
            let althen : StatementSyntax =
                match thenExp,elseExpOpt with
                | Expression (FSALExpr (StatementExpr alStatement)),_ ->
                    let st = [alStatement] |> buildStatements []
                    match st[0] with
                    | :? AssignmentStatementSyntax as s ->
                        s.WithSemicolonToken(SyntaxToken())
                    | _ -> st[0]
                | Expression e, _ ->
                    let expr = e |> buildExpression :?> CodeExpressionSyntax
                    match elseExpOpt with
                    | Some _ ->  sf.ExpressionStatement(expr)
                    | None -> sf.ExpressionStatement(expr).WithSemicolonToken(sf.Token(sk.SemicolonToken))
                    |> (fun f -> f.WithTrailingTrivia(sf.Space))
                | Assignment(assignedTo, expression),Some _ ->
                    ALStatementImpl.createAssignment(assignedTo,expression)
                        .WithSemicolonToken(SyntaxToken())
                | Assignment(assignedTo, expression),None ->
                    ALStatementImpl.createAssignment(assignedTo,expression)
                | _ -> failwithf $"unhandled case"
                
            let alelseopt : (StatementSyntax option) = elseExpOpt |> Option.map (fun f ->
                match f with
                | Expression (FSALExpr (StatementExpr alStatement)) ->
                    let st = [alStatement] |> buildStatements []
                    st[0]
                | Expression e ->
                    let exp = e |> buildExpression :?> CodeExpressionSyntax
                    let st = exp |> sf.ExpressionStatement
                    st.WithSemicolonToken(sf.Token(sk.SemicolonToken))
                | Assignment(assignedTo, expression) ->
                    ALStatementImpl.createAssignment(assignedTo,expression)
                        
                | _ -> failwithf $"unhandled case"
            )
            let statement =
                match alelseopt with
                | None ->
                    sf.IfStatement(alguard,althen)
                        .WithIfKeywordToken(sf.Token(sk.IfKeyword).WithTrailingTrivia(sf.Space))
                        .WithThenKeywordToken(sf.Token(sk.ThenKeyword).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
                        //                .WithSemicolonToken(sf.Token(sk.SemicolonToken))
                | Some elseStatement ->
                    sf.IfStatement(
                        alguard,
                        althen.WithTrailingTrivia(sf.Space),
                        elseStatement)
                        .WithIfKeywordToken(sf.Token(sk.IfKeyword).WithTrailingTrivia(sf.Space))
                        .WithElseKeywordToken(sf.Token(sk.ElseKeyword).WithTrailingTrivia(sf.Space))
                        .WithThenKeywordToken(sf.Token(sk.ThenKeyword).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
                        
            buildStatements (statement::acc) tail

        | Exit alExpression ->
            let alexp = alExpression |> buildExpression :?> CodeExpressionSyntax
            let exitstatement =
                sf.ExitStatement(alexp)
                    .WithOpenParenthesisToken(sf.Token(sk.OpenParenToken))
                    .WithCloseParenthesisToken(sf.Token(sk.CloseParenToken))
                    .WithSemicolonToken(sf.Token(sk.SemicolonToken))
            buildStatements (exitstatement::acc) tail
        | ForLoop(initval, endval, doStatement, isUp) ->
            let loopvar = sf.IdentifierName("i")
            let doStatementExpanded =
                match doStatement with
                | FSALExpr (StatementExpr alStatement) ->
                    match alStatement with
                    | Assignment(alExpression, expression) -> 
                        ALStatementImpl.createAssignment (alExpression,expression)
                    | _ -> failwith "invalid statement"
                | _ -> failwith "invalid statement"
            
            let sad123 = 5
            let l1_idf : CodeExpressionSyntax = loopvar
            let l2_init  =
                match initval with
                | Constant o ->
                    let intv : int = unbox o
                    sf.Literal(intv)
                    |> sf.Int32SignedLiteralValue
                    |> sf.LiteralExpression
                | _ -> failwith ""
            let l4_end = buildExpression endval :?> CodeExpressionSyntax
            let statement =        
                sf.ForStatement(
                    loopvar |> (t.wls >> t.wts),
                    l2_init |> (t.wls >> t.wts),
                    sf.Token(if isUp then sk.ToKeyword else sk.DownToKeyword),
                    l4_end |> (t.wls >> t.wts),
                    doStatementExpanded |> (t.wls >> t.wts))
            buildStatements (statement::acc) tail
        | WhileLoop(fsal_guard, fsal_do) ->
            let al_guard = fsal_guard |> buildExpression :?> CodeExpressionSyntax
            let al_do =
                fsal_do |> ALStatement.ofExpression
                |> (fun f -> buildStatements [] [f] )
                |> List.head
            let statement =
                sf.WhileStatement(al_guard, al_do)
                    .WithDoKeywordToken(sf.Token(sk.DoKeyword) |> (t.wlst >> t.wtst) )
                    .WithWhileKeywordToken(sf.Token(sk.WhileKeyword) |> t.wtst)
            buildStatements (statement::acc) tail
        | Block exprs ->
            let statements =
                buildStatements [] exprs
                |> List.map (fun f ->
                    let currtrivia = f.GetLeadingTrivia()
                    f.WithLeadingTrivia(currtrivia.AddRange(t._4spaces))
                )
            
            let sl = sf.List(statements)
            let statement =
                sf.Block(sl)
                    .WithBeginKeywordToken(sf.Token(sk.BeginKeyword))
                    .WithEndKeywordToken(sf.Token(sk.EndKeyword).WithLeadingTrivia(Trivia.lf8spaces))
                    .WithSemicolonToken(sf.Token(sk.SemicolonToken))
            buildStatements (statement::acc) tail
        | Sequence(alStatement, alStatement2) ->
            let statements =
                buildStatements [] [alStatement;alStatement2]
            buildStatements (statements @ acc) tail
                
        | x -> failwithf $"%A{x}"
    

module Members =
    let createMember (builder:ALMemberBuilder) : MemberSyntax =
        
        match builder with
        | Procedure b ->
            let variables =
                b.localVariables
                |> Seq.map (fun f -> VariableDeclaration.create f.name (TypeReference.create f.altype) )
                |> sf.List
                |> VarSection.create
            let statements =
                b.statements |> Seq.toList
                |> List.where (fun f -> f <> ALStatement.Expression ( FSALExpr Ignore))
                |> buildStatements [] 
            let parameters = b.parameters |> ParameterList.create
            let procbody = Block.create (sf.List(statements))
            Procedure.create b.identifier parameters variables procbody b.returnType
       
    let createField (builder:ALFieldBuilder) : FieldSyntax =
        match builder with
        | RecordField b ->
            Field.create b.id b.identifier b.altype  


module Objects =
    let createALCodeunit (builder:ALObjectBuilder) =
        
        let props =
            match builder.fsharpEntity.TryGetAttribute<ALSingleInstanceCodeunit>() with
            | Some attr ->
                let pv =
                    sf.PropertyLiteral(PropertyKind.SingleInstance,true).WithLeadingTrivia(Trivia.lf4spaces)
                    :> PropertySyntaxOrEmpty
                    |> sf.List
                let pl = sf.PropertyList(pv)
                pl
            | _ -> sf.PropertyList()
        
        let filename = $"Codeunit.{builder.objectId}.{builder.fsharpEntity.DisplayName}.al"
        let alObject = Codeunit.create builder.objectId builder.fsharpEntity.CompiledName
            
        let members =
            builder.alMembers
            |> List.map Members.createMember
            
        let ALCode =
            alObject
                .WithMembers(sf.List(members))
                .WithPropertyList(props)
                .ToFullString()
        
        let writeoutput() =
            Path.Combine(builder.sharedCache.outputPath,filename)
            |> (fun f -> File.WriteAllText(f,ALCode))
            
        writeoutput()
        printfn $"compiled %s{filename}"
        

    let createALRecord (builder:ALObjectBuilder) =
        
        let filename = $"Table.{builder.objectId}.{builder.fsharpEntity.DisplayName}.al"
        let alObject = Record.create builder.objectId builder.fsharpEntity.CompiledName
            
        let members = builder.alMembers |> List.map Members.createMember
        let fields = builder.alFields |> Seq.map Members.createField
            
        let ALCode =
            alObject
                .WithFields(FieldList.create fields)
                .WithMembers(sf.List(members))
                .ToFullString()
        
        let writeoutput() =
            Path.Combine(builder.sharedCache.outputPath,filename)
            |> (fun f -> File.WriteAllText(f,ALCode))
            
        writeoutput()
        printfn $"compiled %s{filename}"
        let debug = 5
        
        ()

let createALObject (builder:ALObjectBuilder) =
    let alType = builder.fsharpEntity |> FSharpEntity.getALObjectKind
    match alType with
    | ALType.Complex
        (ALComplexType.Codeunit name) -> Objects.createALCodeunit builder 
    | ALType.Complex
        (ALComplexType.Record name) -> Objects.createALRecord builder
    | _ -> failwithf $"not implemented type {alType}" 
    
    
module ALObjectBuilder =
    let createALFile (x:ALObjectBuilder) = x |> createALObject
         