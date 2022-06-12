module Fs.AL.SampleProject.CodeunitTest04Loops

open Fs.AL.Core
open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.Abstract
open Fs.AL.Packages
open Fs.AL.SampleProject.RecordTest01

//#nowarn "20"


[<ALSingleInstanceCodeunit(60004)>]
module LoopsCodeunit =
    do()
    // let add2 x = x + 2

    let rec recursiveExample(n: int) =
       let newvalue =
            if n > 1 then
               n + recursiveExample (n - 1)
            else
               n
       newvalue

    let whileLoopExample () = 
        let mutable i = 0
        while i < 5 do
            stdout.WriteLine "hello" 
            i <- i + 1 
    

    
            
    // TODO: reimplement
    // let forEachLoopExample () = 
    //     let list1 = ResizeArray()
    //     list1.Add(1)
    //     list1.Add(2)
    //     list1.Add(3)

    //     for i in list1 do
    //         stdout.WriteLine "hello" 
            
    // let StringConcat (separator : string, stringList : ResizeArray<string>)  = 
    //     let mutable results = "" 
    //     for addr in stringList do
    //         results <- results + separator + addr
    //     results.Substring(1)