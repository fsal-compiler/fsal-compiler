module Fs.AL.SampleProject.CodeunitTest03Constructors

open Fs.AL.Core.ALComplexValues
open Fs.AL.Packages
open Fs.AL.SampleProject.RecordTest01

type private Customer = BaseApplicationALPackage.Tables.Customer
type private Employee = BaseApplicationALPackage.Tables.Employee

#nowarn "20"

type ConstructorsCodeunit() =
    inherit ALCodeunit()
    override this.ObjectId = 60003

    member this.newObject1() =
        let record = SimpleRecord()
        record.Id <- 12
        record.SomeNumber <- 20
        record.SomeNumber

    member this.newObject2() =
        let record2 = SimpleRecord(Id = 15, SomeNumber = 25)
        record2.SomeNumber

    

    member this.newObject3() = 
        SimpleRecord(Id = 12, SomeNumber = 20)

    member this.newObject4() = 
        let rec1 =
            SimpleRecord(
                Id = 12, 
                SomeNumber = 20)
        
        let rec2 = 
            SimpleRecord(Id = 15, SomeNumber = 25)

        let mutable rec3 = rec1
        rec3 <- rec2 

        rec3

    // inner let expression not supported
    // member this.newObject5() =
    //     let record =
    //         let innerRecord = SimpleRecord()
    //         innerRecord.Id <- 12
    //         innerRecord.SomeNumber <- 20
    //         innerRecord
    //     record.SomeNumber


