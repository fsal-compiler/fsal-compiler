module Fs.AL.Compiler.ALCodeGeneration.Nodes

open System
open Fs.AL.Compiler.ALBuilder
open Fs.AL.Compiler.ALCodeGeneration
open Fs.AL.Compiler.ALCodeGeneration.Common
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Compiler.Visitors
open Fs.AL.Core.ALCoreValues
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis.Syntax

type private sf = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxFactory
type private sk = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxKind


module PropertyList =
    
    let create (props:PropertySyntaxOrEmpty seq) =
        sf.PropertyList(
            sf.List(props))
        
    let ofOptionValues (values:string list) : PropertySyntaxOrEmpty seq =
        let proplist = 
            seq {
                let optmempropvalue =
                    values
                    |> SeparatedSyntaxList.ofValues
                    |> sf.OptionValues
                    |> sf.OptionValuesPropertyValue
                yield sf.Property(PropertyKind.OptionMembers, optmempropvalue).WithLeadingTrivia(Trivia.lf12spaces) :> PropertySyntaxOrEmpty
                let optcaptpropvalue =
                    values
                    |> String.concat ","
                    |> sf.Literal
                    |> sf.StringLiteralValue
                    |> sf.StringPropertyValue
                yield sf.Property(PropertyKind.OptionCaption,optcaptpropvalue).WithLeadingTrivia(Trivia.lf12spaces) :> PropertySyntaxOrEmpty
            }    
        proplist


module TypeReference =
    
    module t = Common.Trivia
    let create (name:ALType) : TypeReferenceBaseSyntax =
        match name with
        | Simple (Option values) ->
            let opttype =
                sf.ParseToken("Option") |> sf.OptionDataType
            let vals = SeparatedSyntaxList.ofValues values
            opttype.WithOptionValues(sf.OptionValues vals)
            |> sf.SimpleTypeReference
            :> TypeReferenceBaseSyntax
        | Simple (List lt) ->
            let inner =
                Simple lt
                |> ALType.toString
                |> sf.ParseToken
    //            |> sf.ObjectNameOrId
                |> sf.SimpleNamedDataType
            
            let okt = sf.Token(sk.OfKeyword)|> t.wtst
            let opb = sf.Token(sk.OpenBracketToken)
            let closebrace = sf.Token(sk.CloseBracketToken) 
            let typename = sf.ParseToken("List") |> (t.wlst >> t.wtst)
            let listt =
                sf.GenericNamedDataType(typename)
                    .WithOfKeyword(okt)
                    .WithOpenBracketToken(opb)
                    .WithCloseBracketToken(closebrace)
                    .WithTypeArguments(SeparatedSyntaxList.ofDataTypeSyntax inner)
            listt
            |> sf.SimpleTypeReference
            :> TypeReferenceBaseSyntax
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
        | Complex (Codeunit name) ->
            let coe = sf.IdentifierName $"{name}"
            let objnameorid = sf.ObjectNameOrId(coe)
            let reckeyword = sf.ParseToken("Codeunit").WithTrailingTrivia(sf.Space).WithLeadingTrivia(sf.Space)
            let st = sf.SubtypedDataType(reckeyword,objnameorid)
            sf.SimpleTypeReference(st)
            |> (fun f -> f.WithLeadingTrivia(sf.Space))
        | Simple _ ->
            name
            |> ALType.toString
            |> sf.ParseToken
            |> sf.SimpleNamedDataType
            |> sf.SimpleTypeReference
            |> (fun f -> f.WithLeadingTrivia(sf.Space))
        | Complex _ ->
            name
            |> ALType.toString
            |> sf.ParseToken
            |> sf.SimpleNamedDataType
            |> sf.SimpleTypeReference
            |> (fun f -> f.WithLeadingTrivia(sf.Space))
//        | _ ->
//            name |> string |> NotImplementedException |> raise
//        
        
        
module VariableDeclaration =
    let create (name:string) (typeref:TypeReferenceBaseSyntax) =
        let d = 5
        sf.VariableDeclaration(name,typeref)
        :> VariableDeclarationBaseSyntax
        |> (fun f -> f.WithLeadingTrivia(Trivia.lf8spaces))
        
        
module VarSection =
    let create declarations = sf.VarSection(declarations).WithLeadingTrivia(Trivia.lf4spaces)
    
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
    
    let create isLocal (name:string) parameters variables (body:BlockSyntax) returnval =
        let returnv =
            match returnval with
            | None -> null
            | Some value ->
                sf.ReturnValue(TypeReference.create value)
            
        let nametoken = sf.IdentifierName(name).WithLeadingTrivia(sf.Space)
        sf.MethodDeclaration(nametoken)
            .WithBody(body)
            .WithParameterList(parameters)
            .WithAccessModifier(sf.Token(SyntaxKind.None))
            .WithVariables(variables)
            .WithReturnValue(returnv)
            .WithAccessModifier(
                match isLocal with   
                | true -> sf.Token(sk.LocalKeyword).WithTrailingTrivia(sf.Space)
                | _ -> sf.Token(sk.EmptyToken)
            )
            |> (fun f -> f.WithLeadingTrivia(Trivia.lf4spaces))
            
            


module Codeunit =
    let create (id:int) (name:string) =
        let objId = sf.ObjectId(id).WithLeadingTrivia(sf.Space)
        let objName = sf.IdentifierName(name).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Linefeed)
        let cu = sf.Codeunit(objId,objName)
        cu
            
            
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
            | Simple (Option values) ->
                TypeReference.create (Simple (Option []))
                |> (fun f -> f.DataType)
            | _ ->
                let tr = TypeReference.create altype
                tr.DataType
            | _ ->
                altype |> ALType.toString
                |> sf.ParseToken
                |> sf.SimpleNamedDataType :> DataTypeSyntax
        
        let field =
            sf.Field(id,name,datatype)
            |> (fun f -> f.WithLeadingTrivia(Trivia.lf8spaces))
            |> (fun f -> f.WithOpenBraceToken(f.OpenBraceToken.WithLeadingTrivia(Trivia.lf8spaces)))
            |> (fun f -> f.WithCloseBraceToken(f.CloseBraceToken.WithLeadingTrivia(Trivia.lf8spaces)))
            
        
        let field2 =
            match altype with
            | Simple (Option values) ->
                let proplist = values |> PropertyList.ofOptionValues |> PropertyList.create
                field.WithPropertyList(proplist)
            | _ -> field
                
        field2


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



