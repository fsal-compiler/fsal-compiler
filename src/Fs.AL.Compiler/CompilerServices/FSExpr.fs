namespace Fs.AL.Compiler.CompilerServices

open FSharp.Compiler.Symbols

[<AutoOpen>]
[<RequireQualifiedAccess>]
module FSExpr =
    
    let groupSequentials (_curr:FSharpExpr) (_next:FSharpExpr) =
        let rec chainedSequentials acc next' =
            match next' with
            | FSharpExprPatterns.Sequential(curr,nextExpr) ->
                chainedSequentials (curr :: acc) nextExpr
            | x -> x :: acc
        let chain = chainedSequentials [_curr] _next
        chain |> List.rev
