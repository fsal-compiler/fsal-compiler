module Fs.AL.SampleProject.CodeunitTest04Loops

open Fs.AL.Core
open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.Abstract
open Fs.AL.Packages
open Fs.AL.SampleProject.RecordTest01

//#nowarn "20"


// [<ALSingleInstanceCodeunit(60004)>]
// module SingleInstanceModule =
//     let add2 x = x + 2

//    member this.recursiveExample(n: int) =
//        // note that AL does not support nested let bindings
//        let newvalue =
//            if n > 1 then
//                n + this.recursiveExample (n - 1)
//            else
//                n
//        newvalue

    // member this.recursiveExample2(n: int) =
    //     // note that AL does not support nested let bindings
    //     let newvalue =
    //         if n > 1 then
    //             n + this.recursiveExample (n - 1)
    //         else
    //             n

    //     newvalue