module Fs.AL.Compiler.Logger

open System
let [<Literal>] Error = "Error"
let [<Literal>] Warning = "Warning"
let [<Literal>] Information = "Info"
let [<Literal>] Debug = "Debug"
let mutable current_log_level = Debug

type ILogger = abstract Log : string -> Printf.StringFormat<'a,unit> -> 'a
let ConsoleLogger = { 
  new ILogger with
    member __.Log level format =
//      Printf.kprintf (printfn "[%s][%A] %s" (level) DateTime.Now) format
      Printf.kprintf (printfn "[%s] %s" (level)) format // without time
 }
let logUsing (logger: ILogger) = logger.Log

/// Logs a message using the default logger.
let log level message = logUsing ConsoleLogger level message
let logDebug message =
#if DEBUG     
    logUsing ConsoleLogger Debug message
#endif    
    ()
let logInfo message = logUsing ConsoleLogger Information message
