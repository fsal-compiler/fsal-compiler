namespace Fs.AL.Packages

open Fs.AL
open Fs.AL.Typeprovider

module private Paths =
    let [<Literal>] ``Microsoft_Base Application_19.5.36567.36700`` = __SOURCE_DIRECTORY__ + @"/.alpackages/Microsoft_Base Application_19.5.36567.36700.app"
    let [<Literal>] testapp = __SOURCE_DIRECTORY__  + @"/.alpackages/Test_Cars_19.0.0.0.app"
    
// type TestAppPkg = ALProvider<Path=alPackages.testapp>

//do NOT uncomment this line in an IDE

//type BaseApplicationALPackage = ALProvider<Path=Paths.``Microsoft_Base Application_19.5.36567.36700``>

