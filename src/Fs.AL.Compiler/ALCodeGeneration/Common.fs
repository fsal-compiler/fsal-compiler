module Fs.AL.Compiler.ALCodeGeneration.Common

open System.IO
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis.Syntax

type private sf = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxFactory

module Trivia =
    let _4spaces = [|sf.Space;sf.Space;sf.Space;sf.Space|]
    let lf4spaces = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space|]
    let lf8spaces = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space|]
    let lf12spaces = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space|]

    let trailspace (token:#SyntaxNode) = token.WithTrailingTrivia(sf.Space)
        
    /// with leading space
    let inline wls (token:_) = token.WithLeadingTrivia(sf.Space)
    
    /// with trailing space
    let inline wts (token:_) = token.WithTrailingTrivia(sf.Space)
    
    /// with leading space 
    let inline wlst (token:SyntaxToken) = token.WithLeadingTrivia(sf.Space)
    
    /// with trailing space 
    let inline wtst (token:SyntaxToken) = token.WithTrailingTrivia(sf.Space)
    let wls4 (token:#SyntaxNode) = token.WithLeadingTrivia([|sf.Space;sf.Space;sf.Space;sf.Space|])
    

module SeparatedSyntaxList =
    
    let ofDataTypeSyntax (x:DataTypeSyntax) =
        let ssl = SeparatedSyntaxList()
        ssl.Add(x)
        
    let ofValues (values:string list) =
        let ssl = SeparatedSyntaxList()
        List.fold (fun (acc:SeparatedSyntaxList<IdentifierNameOrEmptySyntax>) (f:string) ->
            f |> sf.IdentifierName |> sf.IdentifierNameOrEmpty |> acc.Add )
            ssl values
        
        