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
      Printf.kprintf (printfn "[%s][%A] %s" (level) DateTime.Now) format
 }
let logUsing (logger: ILogger) = logger.Log

/// Logs a message using the default logger.
let log level message = logUsing ConsoleLogger level message
let logDebug message = logUsing ConsoleLogger Debug message
let logInfo message = logUsing ConsoleLogger Information message
