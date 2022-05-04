module Fs.AL.Compiler.ALCodeGeneration.Nodes

open Fs.AL.Compiler.ALBuilder
open Fs.AL.Compiler.ALCodeGeneration.Common
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALCoreValues
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis.Syntax

type private sf = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxFactory
type private sk = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxKind

module Codeunit =
    let create (id:int) (name:string) =
        let objId = sf.ObjectId(id).WithLeadingTrivia(sf.Space)
        let objName = sf.IdentifierName(name).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Linefeed)
        let cu = sf.Codeunit(objId,objName)
        cu

module TypeReference =
    
    let create (name:ALType) : TypeReferenceBaseSyntax =
        match name with
        | Complex (Record name) ->
            let a = 5
//            let recname = name |> sf.ParseToken
            let coe = sf.IdentifierName $"{name}"
            let objnameorid = sf.ObjectNameOrId(coe)
            let reckeyword = sf.ParseToken("Record").WithTrailingTrivia(sf.Space).WithLeadingTrivia(sf.Space)
//            let reckeyword = sf.Token(sk.RecordTypeReference).WithTrailingTrivia(sf.Space)
            let st = sf.SubtypedDataType(reckeyword,objnameorid)
            let str = sf.RecordTypeReference(st)
            str :> TypeReferenceBaseSyntax
        | _ ->
            name
            |> ALType.toString
            |> sf.ParseToken
//            |> sf.ObjectNameOrId
            |> sf.SimpleNamedDataType
            |> sf.SimpleTypeReference
            |> (fun f -> f.WithLeadingTrivia(sf.Space))
        
        
        
module VariableDeclaration =
    let create (name:string) (typeref:TypeReferenceBaseSyntax) =
        sf.VariableDeclaration(name,typeref)
        :> VariableDeclarationBaseSyntax
        |> (fun f -> f.WithLeadingTrivia(Trivia.lf8spaces))
        
        
module VarSection =
    let create declarations = sf.VarSection(declarations).WithLeadingTrivia(Trivia.lf4spaces)
    

module Block =
    
    let create statements =
        let beginkw = sf.Token(SyntaxKind.BeginKeyword).WithLeadingTrivia(Trivia.lf4spaces)
                          //.WithTrailingTrivia(sf.Linefeed)
        let endkw = sf.Token(SyntaxKind.EndKeyword).WithLeadingTrivia(Trivia.lf4spaces)
        sf.Block()
            .WithBeginKeywordToken(beginkw)
            .WithEndKeywordToken(endkw)
            .WithStatements(statements)
            .WithSemicolonToken(sf.Token(SyntaxKind.SemicolonToken))
            .WithTrailingTrivia(sf.Linefeed)
            

module ParameterList =
    let withSemicolons (x:ParameterListSyntax) =
        let separators = x.Parameters.GetSeparators()
        let newtoken = sf.Token(SyntaxKind.SemicolonToken).WithTrailingTrivia(sf.Space)
        x.ReplaceTokens(separators,(fun a b -> newtoken ))
        
    let create (parameters:ALVariable list) =
        let pm = 5
        parameters
        |> Seq.map (fun f ->
            let p = sf.Parameter(f.name,f.altype |> TypeReference.create)
            if f.isMutable then
                p.WithVarKeyword(sf.Token(sk.VarKeyword).WithTrailingTrivia(sf.Space))
            else p                
        )
        |> SeparatedSyntaxList().AddRange
        |> sf.ParameterList
        |> withSemicolons
        
module Procedure =
    
    let create (name:string) parameters variables body returnval =
        let returnv =
            match returnval with
            | None -> null
            | Some value -> sf.ReturnValue(TypeReference.create value)
            
        let nametoken = sf.IdentifierName(name).WithLeadingTrivia(sf.Space)
        sf.MethodDeclaration(nametoken)
            .WithBody(body)
            .WithParameterList(parameters)
            .WithAccessModifier(sf.Token(SyntaxKind.None))
            .WithVariables(variables)
            .WithLeadingTrivia(Trivia.lf4spaces)
            .WithReturnValue(returnv)
            


            
            
module Record =
    let create (id:int) (name:string) =
        let objId = sf.ObjectId(id).WithLeadingTrivia(sf.Space)
        let objName = sf.IdentifierName(name).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Linefeed)
        let cu =
            sf.Table(objId,objName)
            |> (fun f -> f.WithCloseBraceToken(f.CloseBraceToken.WithLeadingTrivia(sf.Linefeed)))
                
        cu
        
        
module Field =
    
    
    let create (id:int) (name:string) (altype:ALType) =
        
        let a = 5
        let datatype =
            match altype with
            | Simple (Text (Some len))  
            | Simple (Code (Some len)) ->
                let t = 5
                let tok = altype |> ALType.toString |> sf.ParseToken
                let lt =
                    sf.LengthDataType(tok,sf.ParseToken(len|> string))
                        .WithOpenBracketToken(sf.Token(sk.OpenBracketToken))
                        .WithCloseBracketToken(sf.Token(sk.CloseBracketToken))
                        :> DataTypeSyntax
                lt
            | _ ->
                    
                altype |> ALType.toString
                |> sf.ParseToken
                |> sf.SimpleNamedDataType :> DataTypeSyntax
        
        let field =
            sf.Field(id,name,datatype)
            |> (fun f -> f.WithLeadingTrivia(Trivia.lf8spaces))
            |> (fun f -> f.WithOpenBraceToken(f.OpenBraceToken.WithLeadingTrivia(Trivia.lf8spaces)))
            |> (fun f -> f.WithCloseBraceToken(f.CloseBraceToken.WithLeadingTrivia(Trivia.lf8spaces)))
                
        field


module FieldList =
    
    let create (fields:FieldSyntax seq) =
        
        let list =
            fields
            |> sf.List
            |> sf.FieldList
            |> (fun f -> f.WithLeadingTrivia(Trivia.lf4spaces))
            |> (fun f -> f.WithOpenBraceToken(f.OpenBraceToken.WithLeadingTrivia(Trivia.lf4spaces)))
            |> (fun f -> f.WithCloseBraceToken(f.CloseBraceToken.WithLeadingTrivia(Trivia.lf4spaces)))
                
        
        list
