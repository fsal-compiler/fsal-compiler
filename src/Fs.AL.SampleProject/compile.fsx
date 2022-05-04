
open System.Diagnostics

let startinfo =
    let executable = __SOURCE_DIRECTORY__ + "/../Fs.AL.Compiler/bin/Debug/net6.0/Fs.AL.Compiler.exe"
    ProcessStartInfo(executable,"")
    
let proc = new Process(StartInfo = startinfo)
proc.Start()
proc.WaitForExit()