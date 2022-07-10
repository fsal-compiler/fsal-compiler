namespace rec Fs.AL.Core.ALCoreValues

open System
open System.ComponentModel.DataAnnotations
open System.Runtime.CompilerServices
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract

// type system - TODO: add all types

type ALSimpleType =
    | SimpleType of typename:string
    | Text of length: int option
    | Code of length: int option
    | DateTime
    | Boolean
    | Decimal
    | Char
    | Option of values: string list
    | List of listArg : ALSimpleType
    | Integer
    | JsonToken
    | JsonArray
    | JsonObject
    
type ALComplexType =    
    | ComplexType of typename:string
    | Record of name: string
    | Page of name: string
    | Report of name: string
    | Codeunit of name: string
    | Query of name: string
    | Variant 

type ALType =
    | Simple of ALSimpleType
    | Complex of ALComplexType



// wrappers aroung original nav types
type ALRecordId(str:string) =

    do failwith "todo"
    member val Value = str with get, set
    
type ALTestAction(str:string) =
    do failwith "todo"
    member val Value = str with get, set
    
type ALObjectType(str:string) =
    do failwith "todo"


type ALObject(str:string) =
    do failwith "todo"    

type ALTestFilter(str:string) =
    do failwith "todo"
    member val Value = str with get, set
    
    
type ALFormSourceExpressionGetter(str:string) =
    do failwith "todo"
    member val Value = str with get, set
    
    
type ALXmlNamespaceManager() =
    do failwith "todo"
    

type ALXmlNodeList() =
    do failwith "todo"
    
type ALXmlWriteOptions(str:string) =
    do failwith "todo"
    member val Value = str with get, set


type ALXmlDeclaration(str:string) =
    do failwith "todo"
    member val Value = str with get, set

type ALXmlProcessingInstruction(str:string) =
    do failwith "todo"
    member val Value = str with get, set

type ALXmlText(str:string) =
    do failwith "todo"
    member val Value = str with get, set

type ALXmlDocumentType(str:string) =
    do failwith "todo"
    member val Value = str with get, set

type ALClientType =
    | TODO

[<Struct>]
type ALReportFormat =
    | TODO

[<Struct>]
type ALDefaultLayout =
    | TODO

[<Struct>]
type ALTestPermissions =
    | TODO


// common types

[<CompiledName("Code")>]
type ALCode(str:string)=
    inherit ALSimpleValue()
    let mutable _value = str
    
    member this.Value
        with get () = _value
        and set (value:string) =
            this.GetType().GetProperty(nameof this.Value).CustomAttributes
            |> Seq.tryFind (fun f -> f.AttributeType = typeof<MaxLengthAttribute>)
            |> function 
            | None -> _value <- value
            | Some maxlenAttr ->
                let len =  maxlenAttr.ConstructorArguments[0].Value :?> int 
                if value.Length < len
                then _value <- value
                else failwith "data exceeds size limit"
        
    abstract member MaxLength : int 
    default this.MaxLength = Int32.MaxValue
    
    override this.Equals(obj) =
        match obj with
        | :? ALCode as v ->
            _value.Equals(v.Value,comparisonType=StringComparison.OrdinalIgnoreCase)
        | :? string as v ->
            _value.Equals(v,comparisonType=StringComparison.OrdinalIgnoreCase)
        | _ -> false
    override this.GetHashCode() = this.Value.GetHashCode(StringComparison.OrdinalIgnoreCase)
    static member op_Implicit (str:string) = ALCode(str) 
    
//    static member op_Implicit (str:string) = ALCode(str) 



// convenience types

[<ALName("Code")>]

type Code20(str:string) =
    inherit ALCode(str:string)
    
    do if str.Length > 20 then failwith "data exceeds size limit"
    static member op_Implicit (str:string) = Code20(str) 
    
    





//module Extensions =    
//    
//    type System.String with
//        member this.asdasd = 1
