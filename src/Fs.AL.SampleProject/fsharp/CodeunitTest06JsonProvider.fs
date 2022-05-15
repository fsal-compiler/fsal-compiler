module Fs.AL.SampleProject.CodeunitTest06JsonProvider

open Fs.AL.Core.Abstract

type SampleJsonProvider =
    Fable.JsonProvider.Generator<"""{"firstName":"john","lastName":"smith","age":10}""">

[<ALSingleInstanceCodeunit(60006)>]
module JsonProvider =

   let ReadJsonResponse(content:string) =
       let token = SampleJsonProvider(content)
       token.firstName + " " + token.lastName

   let TypeCasting(content:string) =
       let token = SampleJsonProvider(content)
       let ageAsInteger = int token.age
       let ageAsDecimal = token.age
       let ageAsText = string token.age
       let ageIn5Years = ageAsInteger + 5

       ageIn5Years