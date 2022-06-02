[<RequireQualifiedAccess>]
module Fs.AL.Compiler.IntermediateLanguage.ALFunctionReplacements

open Fs.AL.Compiler.Fullname


module FSharpSymbol =
    
    type private t = FSharp.Compiler.Symbols.FSharpSymbol
    let isTypeCast (x:t) =
        match x.FullName with
        | Operators.int -> true
        | _ -> false 