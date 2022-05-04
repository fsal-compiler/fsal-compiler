module Fs.AL.Core.Abstract

open System
open System.Runtime.InteropServices

[<AttributeUsage(AttributeTargets.Struct|||AttributeTargets.Class,Inherited=true,AllowMultiple=false)>]
type ALJson() = 
    inherit System.Attribute()
    
[<AttributeUsage(AttributeTargets.All,Inherited=true,AllowMultiple=false)>]
type ALName(str:string) = 
    inherit System.Attribute()
    member this.Name : string = str


[<AbstractClass>]
type ALSingleInstanceCodeunit(id:int) =
    inherit System.Attribute()
    member this.ObjectId : int = id

//type BcValueAttribute(bcName:string) = 
//    inherit System.Attribute()
//    member this.BcName : string = bcName

//type BcComplexValueAttribute(bcName:string) = 
//    inherit System.Attribute()
//    member this.BcName : string = bcName

[<AbstractClass>]
type ALSimpleValue() = do ()

[<AbstractClass>]
type ALComplexValue() = do ()
    
[<AbstractClass>]    
type ALObjectValue() =
    inherit ALComplexValue()
    abstract member ObjectId : int      

// AL Object types


//[<AbstractClass>]
//type ALRecord() as x =
//    inherit ALObjectValue()
//    
//    member val OnBeforeDelete = Event<ALRecord>()
//    member val OnAfterDelete = Event<ALRecord>()
//    member val OnBeforeInsert = Event<ALRecord>()
//    member val OnAfterInsert = Event<ALRecord>()
//    member val OnBeforeModify = Event<ALRecord>()
//    member val OnAfterModify = Event<ALRecord>()
//    member val OnBeforeRename = Event<ALRecord>()
//    member val OnAfterRename = Event<ALRecord>()
//    member val OnBeforeValidate = Event<ALRecord*int>()
//    member val OnAfterValidate = Event<ALRecord*int>()
//    
    




// Member attributes
[<AttributeUsage(AttributeTargets.Property,Inherited=true,AllowMultiple=false)>]
[<AbstractClass>]
type EventSubscriberAttribute() =
    inherit System.Attribute()