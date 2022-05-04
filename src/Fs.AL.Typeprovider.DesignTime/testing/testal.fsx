


#r @"nuget: Newtonsoft.Json"
#I @"C:\Users\kast\source\myrepos\Fs.AL\src\Fs.AL.Typeprovider.Runtime\bin\Release\net6.0"
// #I @"C:\Users\kast\source\myrepos\Fs.AL.Typeprovider\src\Fs.AL.Typeprovider.Runtime\bin\Release\netstandard2.1"
#r @"Fs.AL.Core.dll"
#r @"ICSharpCode.SharpZipLib.dll" /// PEAB OLEMA RUNTIME DLL KAUSTAS
#r @"Fs.AL.Typeprovider.DesignTime.dll"
#r @"Fs.AL.Typeprovider.Runtime.dll"
#r @"Microsoft.Dynamics.Nav.CodeAnalysis.dll"
open System.Reflection



[<AutoOpenAttribute>]
module Load =
    open System
    // väike app
    let [<Literal>] testApp0 = @"C:\Users\kast\Desktop\alc\alc-klickcopy\.alpackages\Banking Formats localization for Estonia.app"
    // keskmine app
    let [<Literal>] testApp1 = @"C:\Users\kast\Desktop\alc\alc-klickcopy\.alpackages\Balance Statements.app"
    // suur app
    let [<Literal>] testApp2 = @"C:\Users\kast\Desktop\alc\alc-klickcopy\.alpackages\Estonian Personnel365.app"
    // väga suur app
    let [<Literal>] testApp3 = @"C:\Users\kast\Desktop\alc\alc-klickcopy\.alpackages\Base Application.app"



open Fs.AL
open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.Abstract

let [<Literal>] testapp = @"C:\Users\kast\source\myrepos\Fs.AL\src\Fs.AL.Packages.BaseApplication\.alpackages\Test_Cars_19.0.0.0.app"
let [<Literal>] sys = @"C:\Users\kast\source\myrepos\Fs.AL\src\Fs.AL.Packages.BaseApplication\.alpackages\Microsoft_System_19.0.31889.32155.app"



type Provider = ALProvider<Path=testapp>

let table1 = Provider.Tables.Car()

// table1.

// let t1 = Unchecked.defaultof<Provider.Tables.Car>
// let aasd1 = t1.aaaaaaa() 

// ALObjectValue()
// ALRecord()

// table1.ObjectId

// type impl1() =
//     inherit Provider.Tables.Car()
//     override x.ObjectId = 1



// // let customer = prov.Tables.Customer()

// let cust = prov.Tables.``BNK Banking Formats Resource``()





// // let t = prov.Tables.``SBS Balance Statement``()
    


