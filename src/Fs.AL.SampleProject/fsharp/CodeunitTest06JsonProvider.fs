module Fs.AL.SampleProject.CodeunitTest06JsonProvider

open Fs.AL.Core.Abstract

type SampleJsonProvider = 
    Fable.JsonProvider.Generator<"""
{
    "firstName":"john",
    "lastName":"smith",
    "age":10,
    "children": [
        "johnny",
        "timmy",
        "sam",
        "donald"
    ],
    "favoriteNumbers": [
        1,54,654,56,2,21
    ]
}
""">

[<AL.Codeunit(60006)>]
module JsonProvider =
    do()
    let getJsonProp1 (content: string)  = 
        let reader = SampleJsonProvider(content)
        let fname = reader.firstName
        fname

    let getJsonProp2 (content: string)  = 
        let reader = SampleJsonProvider(content)
        let fname2 = reader.firstName + "test" + reader.lastName
        fname2

    let getJsonProp3 (content: string) =
        let token = SampleJsonProvider(content)
        let ageAsDecimal = token.age
        let ageAsInteger = int token.age 
        let ageAsText = string token.age
        ageAsDecimal


    let getJsonProp4 content =
        let token = SampleJsonProvider(content)
        let children = token.children
        let firstChild = children[0]
        firstChild


