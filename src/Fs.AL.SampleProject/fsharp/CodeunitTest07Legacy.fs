module Fs.AL.SampleProject.CodeunitTest07Legacy

open Fs.AL.Core.Abstract

[<ALSingleInstanceCodeunit(60007)>]
module Legacy =
    
   let substrings(content:string) =
       let substring1 = content.Substring(2)
       let someInteger = 6
       let substring2 = content.Substring(someInteger)
       substring2
       