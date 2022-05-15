module Fs.AL.SampleProject.RecordTest02Inheritance

open System
open System.ComponentModel.DataAnnotations
open Fable.Core
open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.ALFunctions
open Fs.AL.Core.ALSimpleValues

#nowarn "20"

// the values won't be initialized in AL
// so we use the default value
let t<'t> = Unchecked.defaultof<'t> 

[<AbstractClass>]
type SharedRecordType() =
    inherit ALRecord()
    member val Id = t<int> with get, set
    [<MaxLength(200)>]
    member val Shared1 = t<string> with get, set
    member val Shared2 = t<int> with get, set
    member this.SharedProcedure() = 
        DateTime.Now

    member this.AssignShared1 (data:string) = 
        let customer = Fs.AL.Packages.BaseApplicationALPackage.Tables.Customer()
        customer.Get(data)
        customer.Address <- "asfdsfgsd"
        customer.Modify() 
        Console.WriteLine "asdasdasd"

    

    
        
type Inherited1() =
    inherit SharedRecordType()
    override this.ObjectId = 60002
    [<MaxLength(500)>]
    member val String = t<string> with get, set


type Inherited2() =
    inherit SharedRecordType()
    override this.ObjectId = 60003
    member val Int = t<int> with get, set




