module Fs.AL.Core.ALFunctions

open System.Runtime.InteropServices

 
module Dialog =
    let Message (txt:string) : unit = printfn $"%s{txt}"
    let Error (txt:string) : unit = eprintfn $"%s{txt}"

    
type Root() =
    static member Evaluate (variable: byref<'t>, str: string, [<Optional>] number: int) : bool = failwith "unimplemented"
    static member Clear (variable: byref<'t>) : unit = failwith "unimplemented"


