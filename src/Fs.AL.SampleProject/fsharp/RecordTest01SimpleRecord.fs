module Fs.AL.SampleProject.RecordTest01

open Fs.AL.Core.ALComplexValues

#nowarn "20"
open Fs.AL.Packages

type private Company = SystemALPackage.Tables.Company

// type SimpleRecord() =
//     inherit ALRecord()
//     override this.ObjectId = 60001

//     member val Id = 0 with get, set
//     member val SomeNumber = 0 with get, set
//     member val SomeNumber2 = 0 with get, set
//     member val SomeNumber3 = 0 with get, set

//     member this.SampleFunction(input: string) =

//         let sum =
//             this.SomeNumber
//             + this.SomeNumber2
//             + this.SomeNumber3
//         // let sum = 
//         //     input
//         //     + "asd"
//         //     + "fgh"

//         sum
