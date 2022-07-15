module Fs.AL.Core.Abstract

open System
open System.Runtime.InteropServices




    
[<AttributeUsage(AttributeTargets.All,Inherited=true,AllowMultiple=false)>]
type ALName(str:string) = 
    inherit System.Attribute()
    member this.Name : string = str



/// AL representations of types
[<RequireQualifiedAccess>]
module AL = 
    
    /// single-instance codeunit for modules
    [<AttributeUsage(AttributeTargets.Struct|||AttributeTargets.Class,Inherited=true,AllowMultiple=false)>]
    type Codeunit(id:int) =
        inherit System.Attribute()
        member this.ObjectId : int = id
        
    /// table for f# record types
    [<AttributeUsage(AttributeTargets.Struct|||AttributeTargets.Class,Inherited=true,AllowMultiple=false)>]     
    type Table(id:int) =
        inherit System.Attribute()
        member this.ObjectId : int = id
    
    /// compile to JsonToken accesses in AL
    [<AttributeUsage(AttributeTargets.Struct|||AttributeTargets.Class,Inherited=true,AllowMultiple=false)>]
    type Json() = 
        inherit System.Attribute()
        
    /// compile to Option values in AL
    [<AttributeUsage(AttributeTargets.Struct|||AttributeTargets.Class,Inherited=true,AllowMultiple=false)>]
    type Option() = 
        inherit System.Attribute()

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