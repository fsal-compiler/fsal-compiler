module Fs.AL.SampleProject.CodeunitTest02Control

open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.ALFunctions

#nowarn "20"
open System

[<Literal>]
let Three = 3

type ControlCodeunit() =
    inherit ALCodeunit()
    override this.ObjectId = 60002

    // member this.ifElseAction(condition: bool) =
    //     if condition then 
    //         Console.WriteLine("true")
    //     else 
    //         Console.WriteLine("false")

    // member this.ifElseReturn(condition: bool) =
    //     if condition then 2 else 7

//    member this.ifElseAssign(condition: bool) =
//        // this compiles into two separate assignments
//        let ifelseassignment =
//            if condition then 2 else 7
//
//        ifelseassignment

    // member this.patternmatch1(input: int) =
    //     let result =
    //         match input with
    //         | 5 -> "case1"
    //         | _ -> "case2"
    //     result

    // member this.patternmatch2(input: int) =
    //     let result =
    //         match input with
    //         | 1 | 2 | 3 -> "case1"
    //         | _ -> "case2"
    //     result

    member this.patternmatch3(someNumber: int) =

        let result =
            match someNumber with
            | 1
            | 2
            | 3 -> "one two or three"
            | v when v > 500 -> "greater than 500"
            | _ -> "other"

        ALDialog.Message("matched result:" + result)

    // member this.patternmatch2 someText =
    //     let result =
    //         match someText with
    //         | "a"
    //         | "b"
    //         | "c" -> "a, b or c"
    //         | nameof someText -> "compile-time constant"
    //         | v when v.StartsWith "d" -> "starts with d"
    //         | v when v.EndsWith "e" -> "ends with e"
    //         | _ -> "other text"

    //     ALDialog.Message("matched result:" + result)
