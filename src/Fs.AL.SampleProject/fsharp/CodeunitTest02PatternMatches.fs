module Fs.AL.SampleProject.CodeunitTest02PatternMatches

open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.ALFunctions

#nowarn "20"

// NOTE:
// pattern-matches are currently supported only for
// let assignments and will fail on unimplemented cases by the compiler

[<Literal>]
let Three = 3

type PatternMatches() =
    inherit ALCodeunit()
    override this.ObjectId = 60002

    member this.patternmatch1(someNumber: int) =

        let result =
            match someNumber with
            | 1
            | 2
            | Three -> "one two or three"
            | v when v > 500 -> "greater than 500"
            | v when v < 0 -> "less than 0"
            | _ -> "other"

        ALDialog.Message("matched result:" + result)

    member this.patternmatch2 someText =
        let result =
            match someText with
            | "a"
            | "b"
            | "c" -> "a, b or c"
            | nameof someText -> "compile-time constant"
            | v when v.StartsWith "d" -> "starts with d"
            | v when v.EndsWith "e" -> "ends with e"
            | _ -> "other text"

        ALDialog.Message("matched result:" + result)
