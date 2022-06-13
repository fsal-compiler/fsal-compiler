namespace rec Fs.AL.Core.ALSimpleValues 

open Fs.AL.Core.Abstract
open System
open System.IO
open System.Collections.Generic
open Fs.AL.Core.ALCoreValues
 
[<CompiledName("ALErrorInfo")>]
type ALALErrorInfo() =
    inherit ALSimpleValue()

    member this.Message : string = failwith "todo" 

    member this.DetailedMessage : string = failwith "todo" 

    member this.TableId : int = failwith "todo" 

    member this.FieldNo : int = failwith "todo" 

    member this.PageNo : int = failwith "todo" 

    member this.ControlName : string = failwith "todo" 

    member this.SystemId : Guid = failwith "todo" 

    member this.RecordId : ALRecordId = failwith "todo" 

    member this.Collectible : bool = failwith "todo" 

    member this.Callstack : string = failwith "todo" 


[<CompiledName("BigText")>]
type ALBigText() =
    inherit ALSimpleValue()

    member this.AddText (bigText:ALBigText,variable:string) : ALBigText = failwith "todo" 

    member this.AddText (bigText:ALBigText,variable:string,toPos1Based:int) : ALBigText = failwith "todo" 

    member this.GetSubText (variable:byref<string>,fromPos1Based:int) : int = failwith "todo" 

    member this.GetSubText (variable:byref<string>,fromPos1Based:int,length:int) : int = failwith "todo" 

    member this.GetSubText (variable:byref<ALBigText>,fromPos1Based:int) : int = failwith "todo" 

    member this.GetSubText (variable:byref<ALBigText>,fromPos1Based:int,length:int) : int = failwith "todo" 

    member this.TextPos (substring:string) : int = failwith "todo" 

    member this.Read (text:byref<ALBigText>,inStream:Stream) : bool = failwith "todo" 

    member this.Write (outStream:Stream) : bool = failwith "todo" 

    member this.Length : int = failwith "todo" 


[<CompiledName("BLOB")>]
type ALBLOB() =
    inherit ALSimpleValue()

    member this.Import (fileName:string) : string = failwith "todo" 

    member this.Export (fileName:string) : string = failwith "todo" 

    member this.CreateInStream ()  : Stream = failwith "todo" 

    member this.CreateInStream (textEncoding:string) : Stream = failwith "todo" 

    member this.CreateInStream (inStream:Stream,textEncoding:string) : unit = failwith "todo" 

    member this.CreateOutStream ()  : Stream = failwith "todo" 

    member this.CreateOutStream (textEncoding:string) : Stream = failwith "todo" 

    member this.CreateOutStream (outStream:Stream,textEncoding:string) : unit = failwith "todo" 

    member this.Assign (other:ALBLOB) : unit = failwith "todo" 

    member this.HasValue : bool = failwith "todo" 

    member this.Length : int = failwith "todo" 


[<CompiledName("HttpHeaders")>]
type ALHttpHeaders() =
    inherit ALSimpleValue()

    member this.Add (key:string,value:string) : bool = failwith "todo" 

    member this.TryAddWithoutValidation (key:string,value:string) : bool = failwith "todo" 

    member this.Contains (key:string) : bool = failwith "todo" 

    member this.Clear ()  : unit = failwith "todo" 

    member this.Remove (key:string) : bool = failwith "todo" 

    member this.GetValues (key:string,resultValues:ResizeArray<'t>) : bool = failwith "todo" 

    member this.Keys ()  : ResizeArray<'t> = failwith "todo" 


[<CompiledName("JsonArray")>]
type ALJsonArray() =
    inherit ALSimpleValue()

    member this.Add (value:ALJsonToken) : unit = failwith "todo" 

    member this.IndexOf (value:ALJsonToken) : int = failwith "todo" 

    member this.IndexOf (value:bool) : int = failwith "todo" 

    member this.IndexOf (value:char) : int = failwith "todo" 

    member this.IndexOf (value:byte) : int = failwith "todo" 

    member this.IndexOf (value:int) : int = failwith "todo" 

    member this.IndexOf (value:decimal) : int = failwith "todo" 

    member this.IndexOf (value:TimeSpan) : int = failwith "todo" 

    member this.IndexOf (value:DateOnly) : int = failwith "todo" 

    member this.IndexOf (value:TimeOnly) : int = failwith "todo" 

    member this.IndexOf (value:DateTime) : int = failwith "todo" 

    member this.IndexOf (value:string) : int = failwith "todo" 

    member this.RemoveAll ()  : unit = failwith "todo" 

    member this.Get (index:int,result:byref<ALJsonToken>) : bool = failwith "todo" 

    member this.Set (index:int,value:ALJsonToken) : bool = failwith "todo" 

    member this.RemoveAt (index:int) : bool = failwith "todo" 

    member this.Insert (index:int,value:ALJsonToken) : bool = failwith "todo" 

    member this.ReadFrom (data:string) : bool = failwith "todo" 

    member this.ReadFrom (data:Stream) : bool = failwith "todo" 

    member this.WriteTo (data:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (data:Stream) : bool = failwith "todo" 

    member this.SelectToken (path:string,result:byref<ALJsonToken>) : bool = failwith "todo" 

    member this.SelectTokens (path:string,result:byref<ResizeArray<'t>>) : bool = failwith "todo" 

    member this.AsArray ()  : ALJsonArray = failwith "todo" 

    member this.AsValue ()  : ALJsonValue = failwith "todo" 

    member this.AsObject ()  : ALJsonObject = failwith "todo" 

    member this.Clone ()  : ALJsonToken = failwith "todo" 

    member this.Clear ()  : unit = failwith "todo" 

    member this.AsToken ()  : ALJsonToken = failwith "todo" 

    member this.Assign (other:ALJsonToken) : unit = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.Path : string = failwith "todo" 

    member this.IsObject : bool = failwith "todo" 

    member this.IsValue : bool = failwith "todo" 

    member this.IsArray : bool = failwith "todo" 


[<CompiledName("JsonObject")>]
type ALJsonObject() =
    inherit ALSimpleValue()

    member this.Contains (key:string) : bool = failwith "todo" 

    member this.Get (key:string,value:byref<ALJsonToken>) : bool = failwith "todo" 

    member this.Add (key:string,value:'t) : bool = failwith "todo" 

    member this.Remove (key:string) : bool = failwith "todo" 

    member this.Replace (key:string,newValue:ALJsonToken) : bool = failwith "todo" 

    member this.Clone ()  : ALJsonToken = failwith "todo" 

    member this.RemoveAll ()  : unit = failwith "todo" 

    member this.ReadFrom (data:string) : bool = failwith "todo" 

    member this.ReadFrom (data:Stream) : bool = failwith "todo" 

    member this.WriteTo (data:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (data:Stream) : bool = failwith "todo" 

    member this.SelectToken (path:string,result:byref<ALJsonToken>) : bool = failwith "todo" 

    member this.SelectTokens (path:string,result:byref<ResizeArray<'t>>) : bool = failwith "todo" 

    member this.AsArray ()  : ALJsonArray = failwith "todo" 

    member this.AsValue ()  : ALJsonValue = failwith "todo" 

    member this.AsObject ()  : ALJsonObject = failwith "todo" 

    member this.Clear ()  : unit = failwith "todo" 

    member this.AsToken ()  : ALJsonToken = failwith "todo" 

    member this.Assign (other:ALJsonToken) : unit = failwith "todo" 

    member this.Keys : ResizeArray<'t> = failwith "todo" 

    member this.Values : ResizeArray<'t> = failwith "todo" 

    member this.Path : string = failwith "todo" 

    member this.IsObject : bool = failwith "todo" 

    member this.IsValue : bool = failwith "todo" 

    member this.IsArray : bool = failwith "todo" 


[<CompiledName("JsonToken")>]
type ALJsonToken() =
    inherit ALSimpleValue()

    member this.ReadFrom (data:string) : bool = failwith "todo" 

    member this.ReadFrom (data:Stream) : bool = failwith "todo" 

    member this.WriteTo (data:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (data:Stream) : bool = failwith "todo" 

    member this.SelectToken (path:string,result:byref<ALJsonToken>) : bool = failwith "todo" 

    member this.SelectTokens (path:string,result:byref<ResizeArray<'t>>) : bool = failwith "todo" 

    member this.AsArray ()  : ALJsonArray = failwith "todo" 

    member this.AsValue ()  : ALJsonValue = failwith "todo" 

    member this.AsObject ()  : ALJsonObject = failwith "todo" 

    member this.Clone ()  : ALJsonToken = failwith "todo" 

    member this.Clear ()  : unit = failwith "todo" 

    member this.AsToken ()  : ALJsonToken = failwith "todo" 

    member this.Assign (other:ALJsonToken) : unit = failwith "todo" 

    member this.Path : string = failwith "todo" 

    member this.IsObject : bool = failwith "todo" 

    member this.IsValue : bool = failwith "todo" 

    member this.IsArray : bool = failwith "todo" 


[<CompiledName("JsonValue")>]
type ALJsonValue() =
    inherit ALSimpleValue()

    member this.AsBoolean ()  : bool = failwith "todo" 

    member this.AsByte ()  : byte = failwith "todo" 

    member this.AsChar ()  : char = failwith "todo" 

    member this.AsOption ()  : ALOption = failwith "todo" 

    member this.AsInteger ()  : int = failwith "todo" 

    member this.AsBigInteger ()  : bigint = failwith "todo" 

    member this.AsDecimal ()  : decimal = failwith "todo" 

    member this.AsText ()  : string = failwith "todo" 

    member this.AsCode ()  : Code20 = failwith "todo" 

    member this.AsDate ()  : DateOnly = failwith "todo" 

    member this.AsDateTime ()  : DateTime = failwith "todo" 

    member this.AsTime ()  : TimeOnly = failwith "todo" 

    member this.AsDuration ()  : TimeSpan = failwith "todo" 

    member this.SetValue (value:bool) : unit = failwith "todo" 

    member this.SetValue (value:int) : unit = failwith "todo" 

    member this.SetValue (value:decimal) : unit = failwith "todo" 

    member this.SetValue (value:string) : unit = failwith "todo" 

    member this.SetValueToUndefined ()  : unit = failwith "todo" 

    member this.SetValueToNull ()  : unit = failwith "todo" 

    member this.SetValue (value:DateOnly) : unit = failwith "todo" 

    member this.SetValue (value:DateTime) : unit = failwith "todo" 

    member this.SetValue (value:TimeOnly) : unit = failwith "todo" 

    member this.SetValue (value:TimeSpan) : unit = failwith "todo" 

    member this.ReadFrom (data:string) : bool = failwith "todo" 

    member this.ReadFrom (data:Stream) : bool = failwith "todo" 

    member this.WriteTo (data:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (data:Stream) : bool = failwith "todo" 

    member this.SelectToken (path:string,result:byref<ALJsonToken>) : bool = failwith "todo" 

    member this.SelectTokens (path:string,result:byref<ResizeArray<'t>>) : bool = failwith "todo" 

    member this.AsArray ()  : ALJsonArray = failwith "todo" 

    member this.AsValue ()  : ALJsonValue = failwith "todo" 

    member this.AsObject ()  : ALJsonObject = failwith "todo" 

    member this.Clone ()  : ALJsonToken = failwith "todo" 

    member this.Clear ()  : unit = failwith "todo" 

    member this.AsToken ()  : ALJsonToken = failwith "todo" 

    member this.Assign (other:ALJsonToken) : unit = failwith "todo" 

    member this.IsNull : bool = failwith "todo" 

    member this.IsUndefined : bool = failwith "todo" 

    member this.Path : string = failwith "todo" 

    member this.IsObject : bool = failwith "todo" 

    member this.IsValue : bool = failwith "todo" 

    member this.IsArray : bool = failwith "todo" 


[<CompiledName("ModuleDependencyInfo")>]
type ALModuleDependencyInfo() =
    inherit ALSimpleValue()

    member this.Id : Guid = failwith "todo" 

    member this.Name : string = failwith "todo" 

    member this.Publisher : string = failwith "todo" 


[<CompiledName("ModuleInfo")>]
type ALModuleInfo() =
    inherit ALSimpleValue()

    member this.Id : Guid = failwith "todo" 

    member this.Name : string = failwith "todo" 

    member this.Publisher : string = failwith "todo" 

    member this.AppVersion : ALVersion = failwith "todo" 

    member this.DataVersion : ALVersion = failwith "todo" 

    member this.Dependencies : ResizeArray<'t> = failwith "todo" 

    member this.PackageId : Guid = failwith "todo" 


[<CompiledName("Notification")>]
type ALNotification() =
    inherit ALSimpleValue()

    member this.AddAction (actionCaption:string,codeunitId:int,functionName:string) : unit = failwith "todo" 

    member this.SetData (keyName:string,keyValue:string) : unit = failwith "todo" 

    member this.GetData (keyName:string) : string = failwith "todo" 

    member this.HasData (keyName:string) : bool = failwith "todo" 

    member this.Send ()  : bool = failwith "todo" 

    member this.Recall ()  : bool = failwith "todo" 

    member this.Assign (other:ALNotification) : unit = failwith "todo" 

    member this.Assign (value:'t) : unit = failwith "todo" 

    member this.Message : string = failwith "todo" 

    member this.Id : Guid = failwith "todo" 


[<CompiledName("Option")>]
type ALOption() =
    inherit ALSimpleValue()

    member this.Names : ResizeArray<'t> = failwith "todo" 

    member this.Ordinals : ResizeArray<'t> = failwith "todo" 


[<CompiledName("SessionSettings")>]
type ALSessionSettings() =
    inherit ALSimpleValue()

    member this.Init ()  : unit = failwith "todo" 

    member this.RequestSessionUpdate (saveSettings:bool) : unit = failwith "todo" 

    member this.Assign (other:ALSessionSettings) : unit = failwith "todo" 

    member this.Assign (value:'t) : unit = failwith "todo" 

    member this.Company : string = failwith "todo" 

    member this.ProfileId : string = failwith "todo" 

    member this.ProfileAppId : Guid = failwith "todo" 

    member this.ProfileSystemScope : bool = failwith "todo" 

    member this.LanguageId : int = failwith "todo" 

    member this.LocaleId : int = failwith "todo" 

    member this.TimeZone : string = failwith "todo" 


[<CompiledName("TestAction")>]
type ALTestAction() =
    inherit ALSimpleValue()

    member this.Invoke ()  : unit = failwith "todo" 

    member this.Enabled ()  : bool = failwith "todo" 

    member this.Visible ()  : bool = failwith "todo" 


[<CompiledName("TestField")>]
type ALTestField() =
    inherit ALSimpleValue()

    member this.Activate ()  : unit = failwith "todo" 

    member this.ValidationErrorCount ()  : int = failwith "todo" 

    member this.GetValidationError ()  : string = failwith "todo" 

    member this.GetValidationError (index:int) : string = failwith "todo" 

    member this.Lookup ()  : unit = failwith "todo" 

//    member this.Lookup (selected:ALRecordRef) : unit = failwith "todo" 

    member this.AssistEdit ()  : unit = failwith "todo" 

    member this.Drilldown ()  : unit = failwith "todo" 

    member this.Invoke ()  : unit = failwith "todo" 

    member this.AssertEquals (value:'t) : unit = failwith "todo" 

    member this.SetValue (value:'t) : unit = failwith "todo" 

    member this.AsInteger ()  : int = failwith "todo" 

    member this.AsBoolean ()  : bool = failwith "todo" 

    member this.AsDecimal ()  : decimal = failwith "todo" 

    member this.AsDate ()  : DateOnly = failwith "todo" 

    member this.AsTime ()  : TimeOnly = failwith "todo" 

    member this.AsDateTime ()  : DateTime = failwith "todo" 

    member this.OptionCount ()  : int = failwith "todo" 

    member this.GetOption (index:int) : string = failwith "todo" 

    member this.Enabled ()  : bool = failwith "todo" 

    member this.Editable ()  : bool = failwith "todo" 

    member this.Visible ()  : bool = failwith "todo" 

    member this.HideValue ()  : bool = failwith "todo" 

    member this.ShowMandatory ()  : bool = failwith "todo" 

    member this.Value : string = failwith "todo" 

    member this.Caption : string = failwith "todo" 


[<CompiledName("TextBuilder")>]
type ALTextBuilder() =
    inherit ALSimpleValue()

    member this.Append (str:string) : bool = failwith "todo" 

    member this.AppendLine ()  : bool = failwith "todo" 

    member this.AppendLine (str:string) : bool = failwith "todo" 

    member this.Clear ()  : unit = failwith "todo" 

    member this.EnsureCapacity (capacity:int) : bool = failwith "todo" 

    member this.Insert (position:int,str:string) : bool = failwith "todo" 

    member this.Remove (startIndex:int,count:int) : bool = failwith "todo" 

    member this.Replace (oldValue:string,newValue:string) : bool = failwith "todo" 

    member this.Replace (oldValue:string,newValue:string,startIndex:int,count:int) : bool = failwith "todo" 

    member this.ToText ()  : string = failwith "todo" 

    member this.ToText (startIndex:int,count:int) : string = failwith "todo" 

    member this.Capacity : int = failwith "todo" 

    member this.Length : int = failwith "todo" 

    member this.MaxCapacity : int = failwith "todo" 


[<CompiledName("Version")>]
type ALVersion() =
    inherit ALSimpleValue()

    member this.Create (value:string) : ALVersion = failwith "todo" 

    member this.Create (major:int,minor:int) : ALVersion = failwith "todo" 

    member this.Create (major:int,minor:int,build:int) : ALVersion = failwith "todo" 

    member this.Create (major:int,minor:int,build:int,revision:int) : ALVersion = failwith "todo" 

    member this.Assign (other:ALVersion) : unit = failwith "todo" 

    member this.Assign (other:'t) : unit = failwith "todo" 

    member this.Major : int = failwith "todo" 

    member this.Minor : int = failwith "todo" 

    member this.Build : int = failwith "todo" 

    member this.Revision : int = failwith "todo" 


[<CompiledName("WebServiceActionContext")>]
type ALWebServiceActionContext() =
    inherit ALSimpleValue()

    member this.AddEntityKey (fieldId:int,value:'t) : bool = failwith "todo" 

    member this.SetObjectType (objectType:ALObjectType) : unit = failwith "todo" 

    member this.GetObjectType ()  : ALObjectType = failwith "todo" 

    member this.GetObjectId ()  : int = failwith "todo" 

    member this.SetObjectId (objectId:int) : unit = failwith "todo" 

    member this.Clear ()  : unit = failwith "todo" 


[<CompiledName("XmlAttribute")>]
type ALXmlAttribute() =
    inherit ALSimpleValue()

    member this.Create (name:string,value:string) : ALXmlAttribute = failwith "todo" 

    member this.Create (localName:string,namespaceUri:string,value:string) : ALXmlAttribute = failwith "todo" 

    member this.CreateNamespaceDeclaration (prefix:string,namespaceUri:string) : ALXmlAttribute = failwith "todo" 

    member this.GetParent (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDocument (result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.Remove ()  : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.WriteTo (result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:Stream) : bool = failwith "todo" 

    member this.WriteTo (result:Stream) : bool = failwith "todo" 

    member this.AsXmlNode ()  : ALXmlNode = failwith "todo" 

    member this.AsXmlAttribute ()  : ALXmlAttribute = failwith "todo" 

    member this.AsXmlCData ()  : ALXmlCData = failwith "todo" 

    member this.AsXmlComment ()  : ALXmlComment = failwith "todo" 

    member this.AsXmlDeclaration ()  : ALXmlDeclaration = failwith "todo" 

    member this.AsXmlDocument ()  : ALXmlDocument = failwith "todo" 

    member this.AsXmlDocumentType ()  : ALXmlDocumentType = failwith "todo" 

    member this.AsXmlElement ()  : ALXmlElement = failwith "todo" 

    member this.AsXmlProcessingInstruction ()  : ALXmlProcessingInstruction = failwith "todo" 

    member this.AsXmlText ()  : ALXmlText = failwith "todo" 

    member this.Name : string = failwith "todo" 

    member this.LocalName : string = failwith "todo" 

    member this.NamespaceUri : string = failwith "todo" 

    member this.NamespacePrefix : string = failwith "todo" 

    member this.IsNamespaceDeclaration : bool = failwith "todo" 

    member this.Value : string = failwith "todo" 

    member this.OuterXml : string = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 


[<CompiledName("XmlAttributeCollection")>]
type ALXmlAttributeCollection() =
    inherit ALSimpleValue()

    member this.Get (index:int,result:byref<ALXmlAttribute>) : bool = failwith "todo" 

    member this.Get (name:string,result:byref<ALXmlAttribute>) : bool = failwith "todo" 

    member this.Get (localName:string,namespaceUri:string,result:byref<ALXmlAttribute>) : bool = failwith "todo" 

    member this.RemoveAll ()  : unit = failwith "todo" 

    member this.Remove (attribute:ALXmlAttribute) : unit = failwith "todo" 

    member this.Remove (name:string) : unit = failwith "todo" 

    member this.Remove (localName:string,namespaceUri:string) : unit = failwith "todo" 

    member this.Set (name:string,value:string) : unit = failwith "todo" 

    member this.Set (localName:string,namespaceUri:string,value:string) : unit = failwith "todo" 

    member this.Count : int = failwith "todo" 


[<CompiledName("XmlCData")>]
type ALXmlCData() =
    inherit ALSimpleValue()

    member this.Create (value:string) : ALXmlCData = failwith "todo" 

    member this.GetParent (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDocument (result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.Remove ()  : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.WriteTo (result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:Stream) : bool = failwith "todo" 

    member this.WriteTo (result:Stream) : bool = failwith "todo" 

    member this.AsXmlNode ()  : ALXmlNode = failwith "todo" 

    member this.AsXmlAttribute ()  : ALXmlAttribute = failwith "todo" 

    member this.AsXmlCData ()  : ALXmlCData = failwith "todo" 

    member this.AsXmlComment ()  : ALXmlComment = failwith "todo" 

    member this.AsXmlDeclaration ()  : ALXmlDeclaration = failwith "todo" 

    member this.AsXmlDocument ()  : ALXmlDocument = failwith "todo" 

    member this.AsXmlDocumentType ()  : ALXmlDocumentType = failwith "todo" 

    member this.AsXmlElement ()  : ALXmlElement = failwith "todo" 

    member this.AsXmlProcessingInstruction ()  : ALXmlProcessingInstruction = failwith "todo" 

    member this.AsXmlText ()  : ALXmlText = failwith "todo" 

    member this.Value : string = failwith "todo" 

    member this.OuterXml : string = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 


[<CompiledName("XmlComment")>]
type ALXmlComment() =
    inherit ALSimpleValue()

    member this.Create (value:string) : ALXmlComment = failwith "todo" 

    member this.GetParent (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDocument (result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.Remove ()  : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.WriteTo (result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:Stream) : bool = failwith "todo" 

    member this.WriteTo (result:Stream) : bool = failwith "todo" 

    member this.AsXmlNode ()  : ALXmlNode = failwith "todo" 

    member this.AsXmlAttribute ()  : ALXmlAttribute = failwith "todo" 

    member this.AsXmlCData ()  : ALXmlCData = failwith "todo" 

    member this.AsXmlComment ()  : ALXmlComment = failwith "todo" 

    member this.AsXmlDeclaration ()  : ALXmlDeclaration = failwith "todo" 

    member this.AsXmlDocument ()  : ALXmlDocument = failwith "todo" 

    member this.AsXmlDocumentType ()  : ALXmlDocumentType = failwith "todo" 

    member this.AsXmlElement ()  : ALXmlElement = failwith "todo" 

    member this.AsXmlProcessingInstruction ()  : ALXmlProcessingInstruction = failwith "todo" 

    member this.AsXmlText ()  : ALXmlText = failwith "todo" 

    member this.Value : string = failwith "todo" 

    member this.OuterXml : string = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 


[<CompiledName("XmlDeclaration")>]
type ALXmlDeclaration() =
    inherit ALSimpleValue()

    member this.Create (version:string,encoding:string,standalone:string) : ALXmlDeclaration = failwith "todo" 

    member this.GetParent (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDocument (result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.Remove ()  : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.WriteTo (result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:Stream) : bool = failwith "todo" 

    member this.WriteTo (result:Stream) : bool = failwith "todo" 

    member this.AsXmlNode ()  : ALXmlNode = failwith "todo" 

    member this.AsXmlAttribute ()  : ALXmlAttribute = failwith "todo" 

    member this.AsXmlCData ()  : ALXmlCData = failwith "todo" 

    member this.AsXmlComment ()  : ALXmlComment = failwith "todo" 

    member this.AsXmlDeclaration ()  : ALXmlDeclaration = failwith "todo" 

    member this.AsXmlDocument ()  : ALXmlDocument = failwith "todo" 

    member this.AsXmlDocumentType ()  : ALXmlDocumentType = failwith "todo" 

    member this.AsXmlElement ()  : ALXmlElement = failwith "todo" 

    member this.AsXmlProcessingInstruction ()  : ALXmlProcessingInstruction = failwith "todo" 

    member this.AsXmlText ()  : ALXmlText = failwith "todo" 

    member this.Encoding : string = failwith "todo" 

    member this.Standalone : string = failwith "todo" 

    member this.Version : string = failwith "todo" 

    member this.OuterXml : string = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 


[<CompiledName("XmlDocument")>]
type ALXmlDocument() =
    inherit ALSimpleValue()

    static member Create ()  : ALXmlDocument = failwith "todo" 

    static member ReadFrom (data:string,result:byref<ALXmlDocument>) : bool = failwith "todo" 

    static member ReadFrom (data:Stream,result:byref<ALXmlDocument>) : bool = failwith "todo" 

    static member ReadFrom (data:string,readOptions:ALXmlReadOptions,result:byref<ALXmlDocument>) : bool = failwith "todo" 

    static member ReadFrom (data:Stream,readOptions:ALXmlReadOptions,result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.GetRoot (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDeclaration (result:byref<ALXmlDeclaration>) : bool = failwith "todo" 

    member this.SetDeclaration (declaration:ALXmlDeclaration) : bool = failwith "todo" 

    member this.GetDocumentType (result:byref<ALXmlDocumentType>) : bool = failwith "todo" 

    member this.RemoveNodes ()  : unit = failwith "todo" 

    member this.GetChildNodes ()  : ALXmlNodeList = failwith "todo" 

    member this.GetChildElements ()  : ALXmlNodeList = failwith "todo" 

    member this.GetChildElements (name:string) : ALXmlNodeList = failwith "todo" 

    member this.GetChildElements (localName:string,namespaceUri:string) : ALXmlNodeList = failwith "todo" 

    member this.GetDescendantNodes ()  : ALXmlNodeList = failwith "todo" 

    member this.GetDescendantElements ()  : ALXmlNodeList = failwith "todo" 

    member this.GetDescendantElements (name:string) : ALXmlNodeList = failwith "todo" 

    member this.GetDescendantElements (localName:string,namespaceUri:string) : ALXmlNodeList = failwith "todo" 

    member this.GetParent (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDocument (result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.Remove ()  : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.WriteTo (result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:Stream) : bool = failwith "todo" 

    member this.WriteTo (result:Stream) : bool = failwith "todo" 

    member this.AsXmlNode ()  : ALXmlNode = failwith "todo" 

    member this.AsXmlAttribute ()  : ALXmlAttribute = failwith "todo" 

    member this.AsXmlCData ()  : ALXmlCData = failwith "todo" 

    member this.AsXmlComment ()  : ALXmlComment = failwith "todo" 

    member this.AsXmlDeclaration ()  : ALXmlDeclaration = failwith "todo" 

    member this.AsXmlDocument ()  : ALXmlDocument = failwith "todo" 

    member this.AsXmlDocumentType ()  : ALXmlDocumentType = failwith "todo" 

    member this.AsXmlElement ()  : ALXmlElement = failwith "todo" 

    member this.AsXmlProcessingInstruction ()  : ALXmlProcessingInstruction = failwith "todo" 

    member this.AsXmlText ()  : ALXmlText = failwith "todo" 

    member this.NameTable : ALXmlNameTable = failwith "todo" 

    member this.OuterXml : string = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 


[<CompiledName("XmlDocumentType")>]
type ALXmlDocumentType() =
    inherit ALSimpleValue()

    member this.Create (name:string) : ALXmlDocumentType = failwith "todo" 

    member this.Create (name:string,publicId:string) : ALXmlDocumentType = failwith "todo" 

    member this.Create (name:string,publicId:string,systemId:string) : ALXmlDocumentType = failwith "todo" 

    member this.Create (name:string,publicId:string,systemId:string,internalSubset:string) : ALXmlDocumentType = failwith "todo" 

    member this.GetName (result:byref<string>) : bool = failwith "todo" 

    member this.SetName (value:string) : bool = failwith "todo" 

    member this.GetSystemId (result:byref<string>) : bool = failwith "todo" 

    member this.SetSystemId (value:string) : bool = failwith "todo" 

    member this.GetInternalSubset (result:byref<string>) : bool = failwith "todo" 

    member this.SetInternalSubset (value:string) : bool = failwith "todo" 

    member this.GetPublicId (result:byref<string>) : bool = failwith "todo" 

    member this.SetPublicId (value:string) : bool = failwith "todo" 

    member this.GetParent (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDocument (result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.Remove ()  : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.WriteTo (result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:Stream) : bool = failwith "todo" 

    member this.WriteTo (result:Stream) : bool = failwith "todo" 

    member this.AsXmlNode ()  : ALXmlNode = failwith "todo" 

    member this.AsXmlAttribute ()  : ALXmlAttribute = failwith "todo" 

    member this.AsXmlCData ()  : ALXmlCData = failwith "todo" 

    member this.AsXmlComment ()  : ALXmlComment = failwith "todo" 

    member this.AsXmlDeclaration ()  : ALXmlDeclaration = failwith "todo" 

    member this.AsXmlDocument ()  : ALXmlDocument = failwith "todo" 

    member this.AsXmlDocumentType ()  : ALXmlDocumentType = failwith "todo" 

    member this.AsXmlElement ()  : ALXmlElement = failwith "todo" 

    member this.AsXmlProcessingInstruction ()  : ALXmlProcessingInstruction = failwith "todo" 

    member this.AsXmlText ()  : ALXmlText = failwith "todo" 

    member this.OuterXml : string = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 


[<CompiledName("XmlElement")>]
type ALXmlElement() =
    inherit ALSimpleValue()

    member this.Create (name:string) : ALXmlElement = failwith "todo" 

    member this.Create (localName:string,namespaceUri:string) : ALXmlElement = failwith "todo" 

    member this.GetNamespaceOfPrefix (prefix:string,result:byref<string>) : bool = failwith "todo" 

    member this.GetPrefixOfNamespace (ns:string,result:byref<string>) : bool = failwith "todo" 

    member this.RemoveAllAttributes ()  : unit = failwith "todo" 

    member this.RemoveAttribute (localName:string) : unit = failwith "todo" 

    member this.RemoveAttribute (localName:string,namespaceUri:string) : unit = failwith "todo" 

    member this.RemoveAttribute (attribute:ALXmlAttribute) : unit = failwith "todo" 

    member this.SetAttribute (name:string,value:string) : unit = failwith "todo" 

    member this.SetAttribute (name:string,namespaceUri:string,value:string) : unit = failwith "todo" 

    member this.Attributes ()  : ALXmlAttributeCollection = failwith "todo" 

    member this.RemoveNodes ()  : unit = failwith "todo" 

    member this.GetChildNodes ()  : ALXmlNodeList = failwith "todo" 

    member this.GetChildElements ()  : ALXmlNodeList = failwith "todo" 

    member this.GetChildElements (name:string) : ALXmlNodeList = failwith "todo" 

    member this.GetChildElements (localName:string,namespaceUri:string) : ALXmlNodeList = failwith "todo" 

    member this.GetDescendantNodes ()  : ALXmlNodeList = failwith "todo" 

    member this.GetDescendantElements ()  : ALXmlNodeList = failwith "todo" 

    member this.GetDescendantElements (name:string) : ALXmlNodeList = failwith "todo" 

    member this.GetDescendantElements (localName:string,namespaceUri:string) : ALXmlNodeList = failwith "todo" 

    member this.GetParent (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDocument (result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.Remove ()  : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.WriteTo (result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:Stream) : bool = failwith "todo" 

    member this.WriteTo (result:Stream) : bool = failwith "todo" 

    member this.AsXmlNode ()  : ALXmlNode = failwith "todo" 

    member this.AsXmlAttribute ()  : ALXmlAttribute = failwith "todo" 

    member this.AsXmlCData ()  : ALXmlCData = failwith "todo" 

    member this.AsXmlComment ()  : ALXmlComment = failwith "todo" 

    member this.AsXmlDeclaration ()  : ALXmlDeclaration = failwith "todo" 

    member this.AsXmlDocument ()  : ALXmlDocument = failwith "todo" 

    member this.AsXmlDocumentType ()  : ALXmlDocumentType = failwith "todo" 

    member this.AsXmlElement ()  : ALXmlElement = failwith "todo" 

    member this.AsXmlProcessingInstruction ()  : ALXmlProcessingInstruction = failwith "todo" 

    member this.AsXmlText ()  : ALXmlText = failwith "todo" 

    member this.HasAttributes : bool = failwith "todo" 

    member this.HasElements : bool = failwith "todo" 

    member this.IsEmpty : bool = failwith "todo" 

    member this.Name : string = failwith "todo" 

    member this.LocalName : string = failwith "todo" 

    member this.NamespaceUri : string = failwith "todo" 

    member this.InnerXml : string = failwith "todo" 

    member this.InnerText : string = failwith "todo" 

    member this.OuterXml : string = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 


[<CompiledName("XmlNamespaceManager")>]
type ALXmlNamespaceManager() =
    inherit ALSimpleValue()

    member this.AddNamespace (prefix:string,uri:string) : unit = failwith "todo" 

    member this.HasNamespace (prefix:string) : bool = failwith "todo" 

    member this.LookupNamespace (prefix:string,result:byref<string>) : bool = failwith "todo" 

    member this.LookupPrefix (uri:string,result:byref<string>) : bool = failwith "todo" 

    member this.RemoveNamespace (prefix:string,uri:string) : unit = failwith "todo" 

    member this.PushScope ()  : unit = failwith "todo" 

    member this.PopScope ()  : bool = failwith "todo" 

    member this.NameTable : ALXmlNameTable = failwith "todo" 


[<CompiledName("XmlNameTable")>]
type ALXmlNameTable() =
    inherit ALSimpleValue()

    member this.Add (key:string) : string = failwith "todo" 

    member this.Get (key:string,result:byref<string>) : bool = failwith "todo" 


[<CompiledName("XmlNode")>]
type ALXmlNode() =
    inherit ALSimpleValue()

    member this.GetParent (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDocument (result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.Remove ()  : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.WriteTo (result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:Stream) : bool = failwith "todo" 

    member this.WriteTo (result:Stream) : bool = failwith "todo" 

    member this.AsXmlNode ()  : ALXmlNode = failwith "todo" 

    member this.AsXmlAttribute ()  : ALXmlAttribute = failwith "todo" 

    member this.AsXmlCData ()  : ALXmlCData = failwith "todo" 

    member this.AsXmlComment ()  : ALXmlComment = failwith "todo" 

    member this.AsXmlDeclaration ()  : ALXmlDeclaration = failwith "todo" 

    member this.AsXmlDocument ()  : ALXmlDocument = failwith "todo" 

    member this.AsXmlDocumentType ()  : ALXmlDocumentType = failwith "todo" 

    member this.AsXmlElement ()  : ALXmlElement = failwith "todo" 

    member this.AsXmlProcessingInstruction ()  : ALXmlProcessingInstruction = failwith "todo" 

    member this.AsXmlText ()  : ALXmlText = failwith "todo" 

    member this.OuterXml : string = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 


[<CompiledName("XmlNodeList")>]
type ALXmlNodeList() =
    inherit ALSimpleValue()

    member this.Get (index:int,node:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.Count : int = failwith "todo" 


[<CompiledName("XmlProcessingInstruction")>]
type ALXmlProcessingInstruction() =
    inherit ALSimpleValue()

    member this.Create (target:string,data:string) : ALXmlProcessingInstruction = failwith "todo" 

    member this.GetData (result:byref<string>) : bool = failwith "todo" 

    member this.SetData (value:string) : bool = failwith "todo" 

    member this.GetTarget (result:byref<string>) : bool = failwith "todo" 

    member this.SetTarget (value:string) : bool = failwith "todo" 

    member this.GetParent (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDocument (result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.Remove ()  : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.WriteTo (result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:Stream) : bool = failwith "todo" 

    member this.WriteTo (result:Stream) : bool = failwith "todo" 

    member this.AsXmlNode ()  : ALXmlNode = failwith "todo" 

    member this.AsXmlAttribute ()  : ALXmlAttribute = failwith "todo" 

    member this.AsXmlCData ()  : ALXmlCData = failwith "todo" 

    member this.AsXmlComment ()  : ALXmlComment = failwith "todo" 

    member this.AsXmlDeclaration ()  : ALXmlDeclaration = failwith "todo" 

    member this.AsXmlDocument ()  : ALXmlDocument = failwith "todo" 

    member this.AsXmlDocumentType ()  : ALXmlDocumentType = failwith "todo" 

    member this.AsXmlElement ()  : ALXmlElement = failwith "todo" 

    member this.AsXmlProcessingInstruction ()  : ALXmlProcessingInstruction = failwith "todo" 

    member this.AsXmlText ()  : ALXmlText = failwith "todo" 

    member this.OuterXml : string = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 


[<CompiledName("XmlReadOptions")>]
type ALXmlReadOptions() =
    inherit ALSimpleValue()
    member this.PreserveWhitespace : bool = failwith "todo" 


[<CompiledName("XmlText")>]
type ALXmlText() =
    inherit ALSimpleValue()

    member this.Create (value:string) : ALXmlText = failwith "todo" 

    member this.GetParent (result:byref<ALXmlElement>) : bool = failwith "todo" 

    member this.GetDocument (result:byref<ALXmlDocument>) : bool = failwith "todo" 

    member this.Remove ()  : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectSingleNode (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNode>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.SelectNodes (xpath:string,namespaceManager:ALXmlNamespaceManager,result:byref<ALXmlNodeList>) : bool = failwith "todo" 

    member this.WriteTo (result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:byref<string>) : bool = failwith "todo" 

    member this.WriteTo (writeOptions:ALXmlWriteOptions,result:Stream) : bool = failwith "todo" 

    member this.WriteTo (result:Stream) : bool = failwith "todo" 

    member this.AsXmlNode ()  : ALXmlNode = failwith "todo" 

    member this.AsXmlAttribute ()  : ALXmlAttribute = failwith "todo" 

    member this.AsXmlCData ()  : ALXmlCData = failwith "todo" 

    member this.AsXmlComment ()  : ALXmlComment = failwith "todo" 

    member this.AsXmlDeclaration ()  : ALXmlDeclaration = failwith "todo" 

    member this.AsXmlDocument ()  : ALXmlDocument = failwith "todo" 

    member this.AsXmlDocumentType ()  : ALXmlDocumentType = failwith "todo" 

    member this.AsXmlElement ()  : ALXmlElement = failwith "todo" 

    member this.AsXmlProcessingInstruction ()  : ALXmlProcessingInstruction = failwith "todo" 

    member this.AsXmlText ()  : ALXmlText = failwith "todo" 

    member this.Value : string = failwith "todo" 

    member this.OuterXml : string = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 


[<CompiledName("XmlWriteOptions")>]
type ALXmlWriteOptions() =
    inherit ALSimpleValue()

    member this.PreserveWhitespace : bool = failwith "todo" 


[<CompiledName("Media")>]
type ALMedia() =
    inherit ALSimpleValue()

    member this.Import (stream:Stream,description:string) : Guid = failwith "todo" 

    member this.Import (stream:Stream,description:string,mimeType:string) : Guid = failwith "todo" 

    member this.Import (stream:Stream,description:string,mimeType:string,fileName:string) : Guid = failwith "todo" 

    member this.Import (fileName:string,description:string) : Guid = failwith "todo" 

    member this.Import (fileName:string,description:string,mimeType:string) : Guid = failwith "todo" 

    member this.ImportWithUrlAccess (stream:Stream,fileName:string) : Guid = failwith "todo" 

    member this.ImportWithUrlAccess (stream:Stream,fileName:string,minutesToExpire:int) : Guid = failwith "todo" 

    member this.Export (fileName:string) : bool = failwith "todo" 

    member this.Export (stream:Stream) : bool = failwith "todo" 

    member this.GetDocumentUrl (documentId:Guid) : string = failwith "todo" 

    member this.MediaId : Guid = failwith "todo" 

    member this.HasValue : bool = failwith "todo" 


[<CompiledName("MediaSet")>]
type ALMediaSet() =
    inherit ALSimpleValue()

    member this.Import (stream:Stream,description:string) : Guid = failwith "todo" 

    member this.Import (stream:Stream,description:string,mimeType:string) : Guid = failwith "todo" 

    member this.Import (stream:Stream,description:string,mimeType:string,fileName:string) : Guid = failwith "todo" 

    member this.Import (fileName:string,description:string) : Guid = failwith "todo" 

    member this.Import (fileName:string,description:string,mimeType:string) : Guid = failwith "todo" 

    member this.Export (fileBaseName:string) : int = failwith "todo" 

    member this.Insert (mediaId:Guid) : bool = failwith "todo" 

    member this.Remove (mediaId:Guid) : bool = failwith "todo" 

    member this.Item (index:int) : Guid = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.MediaId : Guid = failwith "todo" 

    member this.HasValue : bool = failwith "todo" 


[<CompiledName("MediaValueBase")>]
type ALMediaValueBase() =
    inherit ALSimpleValue()

    member this.MediaId : Guid = failwith "todo" 

    member this.HasValue : bool = failwith "todo" 




















































[<CompiledName("OptionNull")>]
type ALOptionNull() =
    inherit ALSimpleValue()

    member this.Names : ResizeArray<'t> = failwith "todo" 

    member this.Ordinals : ResizeArray<'t> = failwith "todo" 








