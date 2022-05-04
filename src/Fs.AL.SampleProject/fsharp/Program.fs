namespace Fs.AL.SampleProject


module Program =
    
    open Fs.AL.SampleProject.Demo01HTTPRequests

    [<EntryPoint>]
    let main argv =
        
        // you can also debug here
        
        let translationmgt = TranslationManagement()
//        translationmgt.SimpleHttpRequest "input"
            
        
        printfn "Hello from F#"
        0