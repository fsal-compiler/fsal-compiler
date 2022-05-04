module Fs.AL.Core.ALFunctions

 

    
module ALDialog =
    
    let Message (txt:string) : unit = printfn $"{txt}"
    let Error (txt:string) : unit = eprintfn $"{txt}"
    
//module ALXmlDocument =
//    
//    let ReadFrom (str:string,r:ALXmlDocument ref) : unit = failwith "unimplementeed"
//    let Create (str: string) : ALXmlDocument = failwith "unimplementeed"
