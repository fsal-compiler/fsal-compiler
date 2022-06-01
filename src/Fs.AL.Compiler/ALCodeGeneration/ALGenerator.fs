module rec Fs.AL.Compiler.ALCodeGeneration.ALGenerator

open System.Collections.ObjectModel
open System.IO
open System.Linq.Expressions
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
    | FSALExpr (StatementExpr (Expression alExpression)) ->
        buildExpression alExpression
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
    
    
    
    let createExit (fsalStatement:ALExpression) =
        let alexp = fsalStatement |> buildExpression :?> CodeExpressionSyntax
        let exitstatement =
            sf.ExitStatement(alexp)
                .WithOpenParenthesisToken(sf.Token(sk.OpenParenToken))
                .WithCloseParenthesisToken(sf.Token(sk.CloseParenToken))
                .WithSemicolonToken(sf.Token(sk.SemicolonToken))
        exitstatement
    let createAssignment (level:int) ((assignedTo:ALExpression),(expression:ALExpression)) =
        let leveltrivia = [| for i = 0 to level do t._4spaces |] |> Array.concat
        let keywordLeading = Array.append t.lf4spaces leveltrivia
        // todo: unwrap assignment trees
        match expression with
        | FSALExpr (StatementExpr (Assignment(origtgt, value))) ->
            let target = assignedTo |> buildExpression :?> CodeExpressionSyntax
            let assignedvalue = value |> buildExpression :?> CodeExpressionSyntax
            let assignStatement =
                sf.AssignmentStatement(target,assignedvalue)
                    .WithAssignmentToken(sf.Token(sk.AssignToken).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
                    .WithSemicolonToken(sf.Token(sk.SemicolonToken))
                    .WithLeadingTrivia(keywordLeading)
            assignStatement
        | FSALExpr (StatementExpr (state)) ->
            let preceding,last = ALStatement.collectSequencesLast state
            let dfdfg = 5
            let target = assignedTo |> buildExpression :?> CodeExpressionSyntax
            let assignedvalue =
                match expression with
                | _ -> expression |> buildExpression :?> CodeExpressionSyntax
            let assignStatement =
                sf.AssignmentStatement(target,assignedvalue)
                    .WithAssignmentToken(sf.Token(sk.AssignToken).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
                    .WithSemicolonToken(sf.Token(sk.SemicolonToken))
                    .WithLeadingTrivia(keywordLeading)
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
                    .WithLeadingTrivia(keywordLeading)
            assignStatement
    let createGuardExpr (level:int) (fsalExpr:ALExpression) : (CodeExpressionSyntax * StatementSyntax list) =
        match fsalExpr with
        | FSALExpr (StatementExpr (Sequence(alStatement, statement))) ->
            let preceding,last =  ALStatement.collectSequencesLast (Sequence(alStatement, statement))
            (ALUnaryOperator.Grouping, ALExpression.ofALStatement last) |> buildUnaryExpression, (preceding |> buildStatements (level) [])
        | n ->
            (ALUnaryOperator.Grouping,n) |> buildUnaryExpression, []
            
    let createBlock level (statements:StatementSyntax list) =
        let leveltrivia = [| for i = 0 to level do t._4spaces |] |> Array.concat
        let blocklevel = Array.append t.lf4spaces leveltrivia
        let sl = sf.List(statements)
        let statement =
            sf.Block(sl)
                .WithBeginKeywordToken(sf.Token(sk.BeginKeyword).WithLeadingTrivia(blocklevel))
                .WithEndKeywordToken(sf.Token(sk.EndKeyword).WithLeadingTrivia(blocklevel))
                .WithSemicolonToken(sf.Token(sk.SemicolonToken))
        statement
        
    let createIfElse (level:int) (guard: ALExpression) (``thenExpRaw`` : ALStatement) (``elseExpOpt`` : ALStatement option) : StatementSyntax=
        let leveltrivia = [| for i = 0 to level do t._4spaces |] |> Array.concat
        let alguard,precedingStatements = createGuardExpr level guard
//        let alguard,statementsBeforeGuard = (ALUnaryOperator.Grouping,guard) |> buildUnaryExpression, []
        let fsafdsf = 5
        let thenExp =
            match thenExpRaw with
            | Expression (FSALExpr (StatementExpr statement)) -> statement 
            | _ -> thenExpRaw //Expression (FSALExpr (StatementExpr 
        let fgdsgfd = 5
        let althen : StatementSyntax =
            match thenExp,elseExpOpt with
            // assign decision tree variables before then expr
            | Assignment(tgt, FSALExpr (StatementExpr(Sequence(seq1, seq2)))),_ ->
                let lastThen, precedingThen =
                    let all = ALStatement.collectSequences [] (Sequence(seq1, seq2))
                    all.Head, all.Tail |> List.rev
                let fdgfdg =5
                let preceding : StatementSyntax list = precedingThen |> buildStatements (level) []
                let last = [Assignment(tgt,lastThen |> ALExpression.ofALStatement)] |> buildStatements (level) []
                let blockStatement = createBlock level (preceding @ last ) 
//                let precedingThen,lastThen = thenStatements[..thenStatements.Length - 2],thenStatements[thenStatements.Length - 1]
//                let st : StatementSyntax list = [lastThen] |> buildStatements (level+1) []
                match elseExpOpt with
                | Some _ -> blockStatement.WithSemicolonToken(sf.Token(sk.None))
                | None ->
                    let nb = 5
                    blockStatement

            | Block alStatements,_ ->
                let statements = alStatements |> buildStatements (level+2) []
                let block = ALStatementImpl.createBlock (level+1) statements
//                let stat = ALStatementImpl.createBlock (level+1) alStatements
//                let st  =
//                    [Block alStatements] |> buildStatements (level+1) []
//                    |> List.head
//                    :?> BlockSyntax 
                match elseExpOpt with
                | Some _ -> block.WithSemicolonToken(sf.Token(sk.None))
                | None -> block
            | Expression e, _ ->
                let expr = e |> buildExpression :?> CodeExpressionSyntax
                match elseExpOpt with
                | Some _ ->  sf.ExpressionStatement(expr)
                | None -> sf.ExpressionStatement(expr).WithSemicolonToken(sf.Token(sk.SemicolonToken))
                |> (fun f -> f.WithTrailingTrivia(sf.Space))
                |> (fun f -> f.WithLeadingTrivia(Array.append [|sf.Linefeed;sf.Linefeed|] t.lf12spaces))
            | Assignment(assignedTo, expression),Some _ ->
                (ALStatementImpl.createAssignment (level + 1) (assignedTo,expression)).WithSemicolonToken(sf.Token(sk.None))
            | Assignment(assignedTo, expression),None ->
                ALStatementImpl.createAssignment (level + 1) (assignedTo,expression)
            | Exit (ifx),elsex ->
                let exit1 = ALStatementImpl.createExit ifx
                match elsex with
                | Some _ -> exit1.WithSemicolonToken(sf.Token(sk.None))
                | None -> exit1
            | _ -> failwithf $"unhandled case"
            
        let alelseopt : (StatementSyntax option) = elseExpOpt |> Option.map (fun f ->
            match f with
            | Expression (FSALExpr (StatementExpr alStatement)) ->
                let st = [alStatement] |> buildStatements (level + 1) []
                st[0]
            | Expression e ->
                let exp = e |> buildExpression :?> CodeExpressionSyntax
                let st = exp |> sf.ExpressionStatement
                st.WithSemicolonToken(sf.Token(sk.SemicolonToken))
            | Assignment(assignedTo, FSALExpr(StatementExpr (IfStatement(guard, th, els)))) ->
                
                match th with
                | Expression alExpression -> 
                    let statement = ALStatement.ofExpression alExpression
                    match statement with
                    | Assignment (tgt,value) ->  
                        let r = createIfElse (level + 1) guard (Assignment(assignedTo,value)) els
                        r
                    | _ -> failwith ""
                | _ -> failwith ""
            
            | Assignment(assignedTo, expression) ->
                ALStatementImpl.createAssignment (level + 1) (assignedTo,expression)
            | Exit (elsex) ->
                let exit2 = ALStatementImpl.createExit elsex
                exit2
            | _ -> failwithf $"unhandled case"
        )
        let statement =
//            let statementLeading = t.lf12spaces |> Array.append leveltrivia
            let keywordLeading = Array.append t.lf4spaces leveltrivia
            match alelseopt with
            | None ->
                sf.IfStatement(alguard,althen)
                    .WithIfKeywordToken(sf.Token(sk.IfKeyword).WithTrailingTrivia(sf.Space))
                    .WithThenKeywordToken(sf.Token(sk.ThenKeyword).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
                    .WithLeadingTrivia(keywordLeading)
            | Some elseStatement ->
                sf.IfStatement(alguard,althen,elseStatement)
                    .WithIfKeywordToken(sf.Token(sk.IfKeyword).WithTrailingTrivia(sf.Space))
                    .WithThenKeywordToken(sf.Token(sk.ThenKeyword).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
                    .WithElseKeywordToken(sf.Token(sk.ElseKeyword).WithTrailingTrivia(sf.Space).WithLeadingTrivia(keywordLeading))
                    .WithLeadingTrivia(keywordLeading)
        match precedingStatements with
        | [] -> statement
        | _ ->
            ALStatementImpl.createBlock (level)
                (precedingStatements @ [statement])

let rec buildStatements (level:int) (acc: StatementSyntax list) (statements:ALStatement list) : StatementSyntax list =
    let leveltrivia = [| for i = 0 to level do t._4spaces |] |> Array.concat
    match statements with
    | [] ->
        acc
        |> List.map (fun f -> f.WithLeadingTrivia(Array.append t.lf4spaces leveltrivia))
        
    | head::tail ->
        match head with
        | Sequence(statement1, statement2) ->
            let s1 = buildStatements level [] [statement1] 
            let s2 = buildStatements level [] [statement2] 
            let nc = (s2 @ s1 @ acc)
            let t = 5
            // add in reverse
            buildStatements level (nc) tail
        | Expression fsalExp ->
            match fsalExp with
            | FSALExpr Ignore ->  buildStatements level acc tail
            | FSALExpr (StatementExpr alStatement) ->
//                let al_exp = ALExpression.ofALStatement alStatement
//                let ALexp = buildExpression fsalExp :?> CodeExpressionSyntax
                let buildstat = buildStatements level [] [alStatement] |> List.head
//                let statement = sf.ExpressionStatement(ALexp).WithSemicolonToken(sf.Token(sk.SemicolonToken)) :> StatementSyntax
                buildStatements level (buildstat::acc) tail
            | NaryExpression alNaryExpression ->
                let ALexp = buildExpression fsalExp :?> CodeExpressionSyntax
                let statement = sf.ExpressionStatement(ALexp).WithSemicolonToken(sf.Token(sk.SemicolonToken)) :> StatementSyntax
                buildStatements level (statement::acc) tail
            | Binary (left,op,right) ->
                let ALexp = buildExpression fsalExp :?> CodeExpressionSyntax
                let statement = sf.ExpressionStatement(ALexp).WithSemicolonToken(sf.Token(sk.SemicolonToken)) :> StatementSyntax
                buildStatements level (statement::acc) tail
            | Identifier s -> buildStatements level (sf.ExpressionStatement(sf.IdentifierName "BUG")::acc) tail
            | Constant null ->
                // TODO: null condition
//                buildStatements (sf.ExpressionStatement(sf.IdentifierName "BUG")::acc) tail
                buildStatements level  (acc) tail
            | Constant g -> buildStatements level (sf.ExpressionStatement(sf.IdentifierName "BUG")::acc) tail
            | _ -> failwith "test"
                
            
        | Assignment(alExpression, FSALExpr (StatementExpr alStatement)) ->
            //todo: unwrap assignment
            let t = 5
            match alStatement with
            | IfStatement(guard, thenExp, elseExpOpt) ->
                let statement = 
                    ALStatementImpl.createIfElse
                       0
                       guard
                       (Assignment(alExpression,ALExpression.ofALStatement thenExp))
                       (elseExpOpt |> Option.map (fun f -> Assignment(alExpression, f |> ALExpression.ofALStatement)))
                buildStatements level (statement::acc) tail
            | Expression (Constant o) ->
                let statement = ALStatementImpl.createAssignment level (alExpression,Constant o)
                buildStatements level (statement::acc) tail
            | _ -> failwith "unhandled case"
            
        | Assignment(alExpression, expression) ->
            let t = 5
            let statement = ALStatementImpl.createAssignment level (alExpression,expression)
            buildStatements level (statement::acc) tail
        | IfStatement(guard, thenExp, elseExpOpt) ->
            let statement = ALStatementImpl.createIfElse level guard thenExp elseExpOpt            
            buildStatements level (statement::acc) tail
        | Exit alExpression ->
            let alexp = alExpression |> buildExpression :?> CodeExpressionSyntax
            let exitstatement =
                sf.ExitStatement(alexp)
                    .WithOpenParenthesisToken(sf.Token(sk.OpenParenToken))
                    .WithCloseParenthesisToken(sf.Token(sk.CloseParenToken))
                    .WithSemicolonToken(sf.Token(sk.SemicolonToken))
            buildStatements level (exitstatement::acc) tail
        | ForLoop(initval, endval, doStatement, isUp) ->
            let loopvar = sf.IdentifierName("i")
            let doStatementExpanded =
                match doStatement with
                | FSALExpr (StatementExpr alStatement) ->
                    match alStatement with
                    | Assignment(alExpression, expression) -> 
                        ALStatementImpl.createAssignment level (alExpression,expression)
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
            buildStatements level (statement::acc) tail
        | WhileLoop(fsal_guard, fsal_do) ->
            let al_guard = fsal_guard |> buildExpression :?> CodeExpressionSyntax
            let al_do =
                fsal_do |> ALStatement.ofExpression
                |> (fun f -> buildStatements level [] [f] )
                |> List.head
            let statement =
                sf.WhileStatement(al_guard, al_do)
                    .WithDoKeywordToken(sf.Token(sk.DoKeyword) |> (t.wlst >> t.wtst) )
                    .WithWhileKeywordToken(sf.Token(sk.WhileKeyword) |> t.wtst)
            buildStatements level (statement::acc) tail
        | Block exprs ->
            let statements =
                buildStatements level [] exprs
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
            buildStatements level (statement::acc) tail
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
                b.statements
                |> List.singleton
                |> buildStatements 0 []
                |> List.rev
                
            let parameters = b.parameters |> ParameterList.create
            let procbody = ALStatementImpl.createBlock -1 statements |> (fun f -> f.WithTrailingTrivia(sf.Linefeed))
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
         