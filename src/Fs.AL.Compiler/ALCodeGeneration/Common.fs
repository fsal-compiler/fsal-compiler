module Fs.AL.Compiler.ALCodeGeneration.Common

open System.IO
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis.Syntax

type private sf = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxFactory

module Trivia =
    let lf4spaces = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space|]
    let lf8spaces = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space|]

    let trailspace (token:#SyntaxNode) = token.WithTrailingTrivia(sf.Space)
