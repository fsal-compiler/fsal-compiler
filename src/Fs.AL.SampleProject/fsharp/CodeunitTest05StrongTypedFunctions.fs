module Fs.AL.SampleProject.CodeunitTest05StrongTypedFunctions

open System
open Fs.AL.Core
open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract
open Fs.AL.Packages
open Fs.AL.SampleProject.RecordTest01

#nowarn "20"

// strongly typed Get functions


type private Customer = BaseApplicationALPackage.Tables.Customer
type private Employee = BaseApplicationALPackage.Tables.Employee
type private ``User Setup`` = BaseApplicationALPackage.Tables.``User Setup``


[<AL.Codeunit(60005)>]
module StrongTypedFunctions =

    let GetSampleCustomerName () =
        let customer = Customer()
        // Get strongly typed by record primary key
        customer.Get("customer1")

        //and the following will not compile
//        customer.Get(1,2,3)
        customer.Name


    let CreateSampleCustomer () =
        let sampleCustomer =
            Customer(``No.`` = "customer1", Name = "John", ``E-Mail`` = "johnsmith@example.com", ``Phone No.`` = "123123123")

        sampleCustomer.Insert()

    let createEmployee (firstname: string) (lastname: string) =
        Employee(
            ``No.`` = lastname,
            ``First Name`` = firstname,
            ``E-Mail`` = firstname + "." + lastname + "@business.com",
            ``Phone No.`` = "123123123"
        )
