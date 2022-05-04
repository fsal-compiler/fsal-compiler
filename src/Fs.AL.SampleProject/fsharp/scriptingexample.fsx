

// simple script file example
#r "nuget: Fs.AL.Typeprovider"
open System.Diagnostics

// import .app file
type OutlookPackage = 
    Fs.AL.ALProvider<Path= "./.alpackages/OutlookAPI.app">

let Example() =
    // use types directly from .app files
    let setup = OutlookPackage.Tables.``Email - Outlook API Setup``()
        
    setup.ClientSecret
    setup.Modify() |> ignore

    
Example() //System.Exception: Name should be aa, got ab