module Fs.AL.SampleProject.CodeunitTest01SimpleMethods

open System
open Fs.AL.Core.ALComplexValues
#nowarn "20"
open Fs.AL.SampleProject.RecordTest01


type SimpleMethods() =
    inherit ALCodeunit()
    override this.ObjectId = 60001
    
    member this.getNumber() = 5
    member this.getText() = "asdasd"
    member this.getTime() = DateTime.Now // replaced with CurrentDateTime() in AL
    member this.getDeclaredMember() =
        let declaredmember = this.getNumber()
        declaredmember


    member this.assignIfElse (condition:bool) =
        // this compiles into two separate assignments
        // note that if else branching without assignment is not yet implemented
        let ifelseassignment = 
            if condition then this.getNumber()
            else 7
        ifelseassignment + 123

    member this.recursiveExample(n:int) =
        // note that AL does not support nested let bindings
        let newvalue = 
            if n > 1 then
                n + this.recursiveExample(n - 1)
            else n
        newvalue

type CallingOtherObjectType() =

    inherit ALCodeunit()
    override this.ObjectId = 60029

    member this.callRecordProcedure() = 
        let instance = SimpleRecord()
        
        let result = instance.SampleFunction("someinput")
        result