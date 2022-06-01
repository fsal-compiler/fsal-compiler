#r "nuget: Fs.AL.Typeprovider"

// import .app file
type OutlookPackage = 
    Fs.AL.ALProvider<Path= "./.alpackages/OutlookAPI.app">

let Example() =
    let setup = OutlookPackage.Tables.``Email - Outlook Account``()
    setup.


    


    
