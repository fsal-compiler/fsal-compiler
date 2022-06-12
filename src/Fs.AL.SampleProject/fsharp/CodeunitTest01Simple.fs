module Fs.AL.SampleProject.CodeunitTest01SimpleMethods

open Fs.AL

#nowarn "20"
open Fs.AL.Core.ALComplexValues
open System

type SimpleCodeunit() =
    inherit ALCodeunit()
    override this.ObjectId = 60001

    member this.getNumber() = 5
    member this.getText() = "asdasd"
    // member this.getTime() = DateTime.Now // replaced with CurrentDateTime() in AL

    // member this.getDeclaredMember() =
    //     let declaredmember = this.getNumber()
    //     declaredmember // compiles to exit(declaredmember)

    // member this.assignment (input : int) =
    //     let value = input
        
    //     let value3 = 5
    //     let mutable value2 = input 
    //     value2 <- 5 
    //     value2 <- 10
    //     value
    

