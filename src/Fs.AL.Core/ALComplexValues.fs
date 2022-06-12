namespace rec Fs.AL.Core.ALComplexValues 

open Fs.AL.Core.Abstract
open System
open System.IO
open System.Collections.Generic
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.ALSimpleValues
 
[<CompiledName("Automation")>]
type ALAutomation() =
    inherit ALComplexValue()
    
    member this.Assign (other:ALAutomation) : unit = failwith "todo" 

    member this.Assign (other:obj) : unit = failwith "todo" 

    member this.ByValue ()  : ALAutomation = failwith "todo" 


[<CompiledName("DotNet")>]
type ALDotNet() =
    inherit ALComplexValue()
    
    member this.Assign (other:ALDotNet) : unit = failwith "todo" 

    member this.Assign (other:ALDotNet,inheritRunOnClient:bool) : unit = failwith "todo" 

    member this.Assign (value:ALObject) : unit = failwith "todo" 

    member this.Assign (value:obj) : unit = failwith "todo" 

    member this.ByValue ()  : ALDotNet = failwith "todo" 


[<CompiledName("FieldRef")>]
type ALFieldRef() =
    inherit ALComplexValue()
    
    member this.OptionValueCount ()  : int = failwith "todo" 

    member this.GetOptionValueName (index:int) : string = failwith "todo" 

    member this.GetEnumValueNameFromOrdinalValue (ordinal:int) : string = failwith "todo" 

    member this.GetOptionValueCaption (index:int) : string = failwith "todo" 

    member this.GetEnumValueCaptionFromOrdinalValue (ordinal:int) : string = failwith "todo" 

    member this.GetOptionValueOrdinal (index:int) : int = failwith "todo" 

    member this.Assign (other:ALFieldRef) : unit = failwith "todo" 

    member this.ByValue ()  : ALFieldRef = failwith "todo" 

    member this.CalcField ()  : bool = failwith "todo" 

    member this.CalcSum ()  : bool = failwith "todo" 

    member this.SetRange ()  : unit = failwith "todo" 

    member this.SetRange (fromValue:'t) : unit = failwith "todo" 

    member this.SetRange (fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetFilter (expression:string,values:'t[]) : unit = failwith "todo" 

    member this.TestField (errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValue (testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (errorMessage:string) : unit = failwith "todo" 

    member this.Validate (callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Validate (newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe ()  : unit = failwith "todo" 

    member this.ValidateSafe (newValue:'t) : unit = failwith "todo" 

    member this.Number : int = failwith "todo" 

    member this.Type : ALType = failwith "todo" 

    member this.Length : int = failwith "todo" 

    member this.Name : string = failwith "todo" 

    member this.Caption : string = failwith "todo" 

    member this.OptionString : string = failwith "todo" 

    member this.OptionCaption : string = failwith "todo" 

    member this.Active : bool = failwith "todo" 

    member this.Value : 't = failwith "todo" 

    member this.Relation : int = failwith "todo" 

    member this.GetFilter : string = failwith "todo" 

    member this.GetRangeMin : 't = failwith "todo" 

    member this.GetRangeMax : 't = failwith "todo" 


[<CompiledName("File")>]
type ALFile() =
    inherit ALComplexValue()
    
    member this.CreateInStream ()  : Stream = failwith "todo" 

    member this.CreateInStream (inStream:Stream) : unit = failwith "todo" 

    member this.CreateOutStream ()  : Stream = failwith "todo" 

    member this.CreateOutStream (outStream:Stream) : unit = failwith "todo" 

    member this.Create (createName:string) : bool = failwith "todo" 

    member this.Create (createName:string,encoding:string) : bool = failwith "todo" 

    member this.CreateTempFile ()  : bool = failwith "todo" 

    member this.CreateTempFile (encoding:string) : bool = failwith "todo" 

    member this.Open (openName:string) : bool = failwith "todo" 

    member this.Open (openName:string,encoding:string) : bool = failwith "todo" 

    member this.Close ()  : unit = failwith "todo" 

    member this.Seek (position:int) : unit = failwith "todo" 

    member this.Trunc ()  : unit = failwith "todo" 

    member this.IsPathTemporary (filePath:string) : bool = failwith "todo" 

    member this.Erase (fileName:string) : bool = failwith "todo" 

    member this.Rename (oldFileName:string,newFileName:string) : bool = failwith "todo" 

    member this.Copy (fromFileName:string,toFileName:string) : bool = failwith "todo" 

    member this.UploadIntoStream (fromTypeFilter:string,navInStream:byref<Stream>,automationId:Guid) : bool = failwith "todo" 

    member this.UploadIntoStream (dialogTitle:string,fromInitialFolder:string,fromTypeFilter:string,fromFileName:byref<string>,navInStream:byref<Stream>,automationId:Guid) : bool = failwith "todo" 

    member this.Upload (dialogTitle:string,fromInitialFolder:string,fromTypeFilter:string,fromFileName:string,toFileName:byref<string>,automationId:Guid) : bool = failwith "todo" 

    member this.DownloadFromStream (source:Stream,dialogTitle:string,toInitialFolder:string,toTypeFilter:string,toFileName:byref<string>,automationId:Guid) : bool = failwith "todo" 

    member this.Download (fromFileName:string,dialogTitle:string,toInitialFolder:string,toTypeFilter:string,toFileName:byref<string>,automationId:Guid) : bool = failwith "todo" 

    member this.Exists (fileName:string) : bool = failwith "todo" 

    member this.GetStamp (fileName:string,date:byref<DateOnly>) : bool = failwith "todo" 

    member this.GetStamp (fileName:string,date:byref<DateOnly>,time:byref<TimeOnly>) : bool = failwith "todo" 

    member this.SetStamp (fileName:string,date:DateOnly) : bool = failwith "todo" 

    member this.SetStamp (fileName:string,date:DateOnly,time:TimeOnly) : bool = failwith "todo" 

    member this.Assign (other:ALFile) : unit = failwith "todo" 

    member this.Name : string = failwith "todo" 

    member this.Pos : int = failwith "todo" 

    member this.Len : int = failwith "todo" 

    member this.WriteMode : bool = failwith "todo" 

    member this.TextMode : bool = failwith "todo" 


[<CompiledName("FilterPageBuilder")>]
type ALFilterPageBuilder() =
    inherit ALComplexValue()
    
    member this.AddTable (name:string,tableId:int) : string = failwith "todo" 

    member this.AddRecord (name:string,record:ALRecord) : string = failwith "todo" 

    member this.AddRecordRef (name:string,recordRef:ALRecordRef) : string = failwith "todo" 

    member this.AddField (name:string,fieldNo:int) : bool = failwith "todo" 

    member this.AddField (name:string,fieldNo:int,defaultFilter:string) : bool = failwith "todo" 

    member this.GetView (name:string) : string = failwith "todo" 

    member this.GetView (name:string,useCaptions:bool) : string = failwith "todo" 

    member this.SetView (name:string,view:string) : bool = failwith "todo" 

    member this.RunModal ()  : bool = failwith "todo" 

    member this.Assign (other:ALFilterPageBuilder) : unit = failwith "todo" 

    member this.Name (index:int) : string = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.PageCaption : string = failwith "todo" 


[<CompiledName("InStream")>]
type ALInStream() =
    inherit ALComplexValue()
    
    member this.Assign (other:Stream) : unit = failwith "todo" 

    member this.IsEOS : bool = failwith "todo" 


[<CompiledName("KeyRef")>]
type ALKeyRef() =
    inherit ALComplexValue()
    
    member this.Assign (other:ALKeyRef) : unit = failwith "todo" 

    member this.FieldCount : int = failwith "todo" 

    member this.Active : bool = failwith "todo" 

    member this.ALPrimaryKeyIndex : int = failwith "todo" 


[<CompiledName("OutStream")>]
type ALOutStream() =
    inherit ALComplexValue()
    
    member this.Assign (other:Stream) : unit = failwith "todo" 


[<CompiledName("Query")>]
type ALQuery() =
    inherit ALComplexValue()
    
    member this.SaveAsXml (queryId:int,fileName:string) : bool = failwith "todo" 

    member this.SaveAsXml (queryId:int,outStream:Stream) : bool = failwith "todo" 

    member this.SaveAsExcel (queryId:int,fileName:string) : bool = failwith "todo" 

    member this.SaveAsCsv (queryId:int,fileName:string) : bool = failwith "todo" 

    member this.SaveAsCsv (queryId:int,fileName:string,fixedFormat:int) : bool = failwith "todo" 

    member this.SaveAsCsv (queryId:int,fileName:string,fixedFormat:int,formatArgument:string) : bool = failwith "todo" 

    member this.SaveAsCsv (queryId:int,outStream:Stream) : bool = failwith "todo" 

    member this.SaveAsCsv (queryId:int,outStream:Stream,fixedFormat:int) : bool = failwith "todo" 

    member this.SaveAsCsv (queryId:int,outStream:Stream,fixedFormat:int,formatArgument:string) : bool = failwith "todo" 

    member this.SaveAsXml (fileName:string) : bool = failwith "todo" 

    member this.SaveAsXml (outStream:Stream) : bool = failwith "todo" 

    member this.SaveAsExcel (fileName:string) : bool = failwith "todo" 

    member this.SaveAsCsv (fileName:string) : bool = failwith "todo" 

    member this.SaveAsCsv (fileName:string,fixedFormat:int) : bool = failwith "todo" 

    member this.SaveAsCsv (fileName:string,fixedFormat:int,formatArgument:string) : bool = failwith "todo" 

    member this.SaveAsCsv (outStream:Stream) : bool = failwith "todo" 

    member this.SaveAsCsv (outStream:Stream,fixedFormat:int) : bool = failwith "todo" 

    member this.SaveAsCsv (outStream:Stream,fixedFormat:int,formatArgument:string) : bool = failwith "todo" 

    member this.ColumnName (columnNo:int) : string = failwith "todo" 

    member this.ColumnCaption (columnNo:int) : string = failwith "todo" 

    member this.ColumnNo (columnNo:int) : int = failwith "todo" 

    member this.SetFilter (columnNo:int,expression:string,values:'t[]) : unit = failwith "todo" 

    member this.SetRangeSafe (columnNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.SetRangeSafe (columnNo:int,expectedType:ALType,fromValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (columnNo:int,expectedType:ALType,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.GetFilter (columnNo:int) : string = failwith "todo" 

    member this.Open ()  : bool = failwith "todo" 

    member this.Read ()  : bool = failwith "todo" 

    member this.Close ()  : unit = failwith "todo" 

    member this.GetFilters : string = failwith "todo" 

    member this.SkipNumberOfRows : int = failwith "todo" 

    member this.TopNumberOfRowsToReturn : int = failwith "todo" 

 


[<CompiledName("RecordRef")>]
type ALRecordRef() =
    inherit ALComplexValue()
    
    member this.Open (tableId:int) : unit = failwith "todo" 

    member this.Open (tableId:int,isTemporary:bool) : unit = failwith "todo" 

    member this.Open (tableId:int,isTemporary:bool,companyName:string) : unit = failwith "todo" 

    member this.Close ()  : unit = failwith "todo" 

    member this.Init ()  : unit = failwith "todo" 

    member this.AddLink (url:string) : int = failwith "todo" 

    member this.AddLink (url:string,description:string) : int = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecordRef) : unit = failwith "todo" 

    member this.CopyLinks (fromRecordOrRecordRef:ALVariant) : unit = failwith "todo" 

    member this.Delete ()  : unit = failwith "todo" 

    member this.Delete (runTrigger:bool) : unit = failwith "todo" 

    member this.DeleteAll (runTrigger:bool) : unit = failwith "todo" 

    member this.DeleteAll ()  : unit = failwith "todo" 

    member this.DeleteLink (linkId:int) : unit = failwith "todo" 

    member this.DeleteLinks ()  : unit = failwith "todo" 

    member this.Assign (other:ALRecordRef) : unit = failwith "todo" 

    member this.Find (searchMethod:string) : unit = failwith "todo" 

    member this.Find ()  : bool = failwith "todo" 

    member this.FindFirst ()  : unit = failwith "todo" 

    member this.FindLast ()  : bool = failwith "todo" 

    member this.FindSet ()  : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool,updateKey:bool) : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool) : unit = failwith "todo" 

    member this.FieldExists (fieldNo:int) : bool = failwith "todo" 

    member this.Get (recordId:ALRecordId) : bool = failwith "todo" 

    member this.GetBySystemId (systemId:Guid) : bool = failwith "todo" 

    member this.GetFilter (fieldNo:int) : string = failwith "todo" 

    member this.GetPosition ()  : string = failwith "todo" 

    member this.GetPosition (useCaptions:bool) : string = failwith "todo" 

    member this.GetTable (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.GetView ()  : string = failwith "todo" 

    member this.GetView (useCaptions:bool) : string = failwith "todo" 

    member this.Insert ()  : unit = failwith "todo" 

    member this.Insert (runTrigger:bool) : bool = failwith "todo" 

    member this.Insert (runTrigger:bool,insertWithSystemId:bool) : bool = failwith "todo" 

    member this.LockTable ()  : unit = failwith "todo" 

    member this.LockTable (wait:bool) : unit = failwith "todo" 

    member this.LockTable (wait:bool,versionCheck:bool) : unit = failwith "todo" 

    member this.Modify ()  : unit = failwith "todo" 

    member this.Modify (runTrigger:bool) : unit = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.Next ()  : int = failwith "todo" 

    member this.Next (steps:int) : int = failwith "todo" 

    member this.Rename (values:'t[]) : unit = failwith "todo" 

    member this.Reset ()  : unit = failwith "todo" 

    member this.SetPermissionFilter ()  : unit = failwith "todo" 

    member this.SetPosition (position:string) : unit = failwith "todo" 

    member this.SetRecFilter ()  : unit = failwith "todo" 

    member this.SetTable (toRecord:ALRecord) : unit = failwith "todo" 

    member this.SetView (view:string) : unit = failwith "todo" 

    member this.ChangeCompany ()  : bool = failwith "todo" 

    member this.ChangeCompany (companyName:string) : bool = failwith "todo" 

    member this.Copy (fromRecord:ALRecord,shareTableHandle:bool) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecordRef) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecordRef,shareTableHandle:bool) : unit = failwith "todo" 

    member this.AddLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.SetLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AreFieldsLoaded (fields:int[]) : bool = failwith "todo" 

    member this.LoadFields (fields:int[]) : bool = failwith "todo" 

    member this.Mark ()  : bool = failwith "todo" 

    member this.Mark (markValue:bool) : bool = failwith "todo" 

    member this.ClearMarks ()  : unit = failwith "todo" 

    member this.Ascending : bool = failwith "todo" 

    member this.Caption : string = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.CountApprox : int = failwith "todo" 

    member this.CurrentKey : string = failwith "todo" 

    member this.CurrentKeyIndex : int = failwith "todo" 

    member this.FieldCount : int = failwith "todo" 

    member this.FilterGroup : int = failwith "todo" 

    member this.GetFilters : string = failwith "todo" 

    member this.HasFilter : bool = failwith "todo" 

    member this.HasLinks : bool = failwith "todo" 

    member this.IsEmpty : bool = failwith "todo" 

    member this.IsDirty : bool = failwith "todo" 

    member this.KeyCount : int = failwith "todo" 

    member this.Name : string = failwith "todo" 

    member this.Number : int = failwith "todo" 

    member this.ReadConsistency : bool = failwith "todo" 

    member this.ReadPermission : bool = failwith "todo" 

    member this.RecordId : ALRecordId = failwith "todo" 

    member this.RecordLevelLocking : bool = failwith "todo" 

    member this.WritePermission : bool = failwith "todo" 

    member this.IsTemporary : bool = failwith "todo" 

    member this.CurrentCompany : string = failwith "todo" 

    member this.SystemIdNo : int = failwith "todo" 

    member this.SystemCreatedAtNo : int = failwith "todo" 

    member this.SystemCreatedByNo : int = failwith "todo" 

    member this.SystemModifiedAtNo : int = failwith "todo" 

    member this.SystemModifiedByNo : int = failwith "todo" 

    member this.MarkedOnly : bool = failwith "todo" 


[<CompiledName("TestPageBase")>]
type ALTestPageBase() =
    inherit ALComplexValue()
    
    member this.ValidationErrorCount ()  : int = failwith "todo" 

    member this.GetValidationError ()  : string = failwith "todo" 

    member this.GetValidationError (index:int) : string = failwith "todo" 

    member this.New ()  : unit = failwith "todo" 

    member this.Next ()  : bool = failwith "todo" 

    member this.View ()  : ALTestAction = failwith "todo" 

    member this.Edit ()  : ALTestAction = failwith "todo" 

    member this.Previous ()  : bool = failwith "todo" 

    member this.First ()  : bool = failwith "todo" 

    member this.Last ()  : bool = failwith "todo" 

    member this.GoToRecord (record:ALRecord) : bool = failwith "todo" 

    member this.GoToKey (values:'t[]) : bool = failwith "todo" 

    member this.FindFirstField (fieldNo:int,value:'t) : bool = failwith "todo" 

    member this.FindNextField (fieldNo:int,value:'t) : bool = failwith "todo" 

    member this.FindPreviousField (fieldNo:int,value:'t) : bool = failwith "todo" 

    member this.Expand (expand:bool) : bool = failwith "todo" 

    member this.Caption : string = failwith "todo" 

    member this.Filter : ALTestFilter = failwith "todo" 

    member this.IsExpanded : bool = failwith "todo" 

    member this.Editable : bool = failwith "todo" 


[<CompiledName("TestPart")>]
type ALTestPart() =
    inherit ALComplexValue()
    
    member this.ValidationErrorCount ()  : int = failwith "todo" 

    member this.GetValidationError ()  : string = failwith "todo" 

    member this.GetValidationError (index:int) : string = failwith "todo" 

    member this.New ()  : unit = failwith "todo" 

    member this.Next ()  : bool = failwith "todo" 

    member this.View ()  : ALTestAction = failwith "todo" 

    member this.Edit ()  : ALTestAction = failwith "todo" 

    member this.Previous ()  : bool = failwith "todo" 

    member this.First ()  : bool = failwith "todo" 

    member this.Last ()  : bool = failwith "todo" 

    member this.GoToRecord (record:ALRecord) : bool = failwith "todo" 

    member this.GoToKey (values:'t[]) : bool = failwith "todo" 

    member this.FindFirstField (fieldNo:int,value:'t) : bool = failwith "todo" 

    member this.FindNextField (fieldNo:int,value:'t) : bool = failwith "todo" 

    member this.FindPreviousField (fieldNo:int,value:'t) : bool = failwith "todo" 

    member this.Expand (expand:bool) : bool = failwith "todo" 

    member this.Caption : string = failwith "todo" 

    member this.Filter : ALTestFilter = failwith "todo" 

    member this.IsExpanded : bool = failwith "todo" 

    member this.Editable : bool = failwith "todo" 


[<CompiledName("Variant")>]
type ALVariant() =
    inherit ALComplexValue()
    
    member this.Assign (other:byte) : unit = failwith "todo" 

    member this.Assign (other:char) : unit = failwith "todo" 

    member this.Assign (other:bool) : unit = failwith "todo" 

    member this.Assign (other:ALOption) : unit = failwith "todo" 

    member this.Assign (other:int) : unit = failwith "todo" 

    member this.Assign (other:uint) : unit = failwith "todo" 

    member this.Assign (other:TimeSpan) : unit = failwith "todo" 

    member this.Assign (other:decimal) : unit = failwith "todo" 

    member this.Assign (other:Guid) : unit = failwith "todo" 

    member this.Assign (other:string) : unit = failwith "todo" 

    member this.Assign (other:ALFilterPageBuilder) : unit = failwith "todo" 

    member this.Assign (other:ALObjectType) : unit = failwith "todo" 

    member this.Assign (other:ALClientType) : unit = failwith "todo" 

    member this.Assign (other:ALReportFormat) : unit = failwith "todo" 

    member this.Assign (other:ALDefaultLayout) : unit = failwith "todo" 

    member this.Assign (other:ALTestPermissions) : unit = failwith "todo" 

    member this.Assign (other:ALType) : unit = failwith "todo" 

    member this.Assign (other:obj) : unit = failwith "todo" 

    member this.ByValue ()  : ALVariant = failwith "todo" 

    member this.IsAction : bool = failwith "todo" 

    member this.IsAutomation : bool = failwith "todo" 

    member this.IsDotNet : bool = failwith "todo" 

    member this.IsBigInteger : bool = failwith "todo" 

    member this.IsBigText : bool = failwith "todo" 

    member this.IsBinary : bool = failwith "todo" 

    member this.IsBoolean : bool = failwith "todo" 

    member this.IsCode : bool = failwith "todo" 

    member this.IsByte : bool = failwith "todo" 

    member this.IsCodeunit : bool = failwith "todo" 

    member this.IsDate : bool = failwith "todo" 

    member this.IsDateFormula : bool = failwith "todo" 

    member this.IsDateTime : bool = failwith "todo" 

    member this.IsDecimal : bool = failwith "todo" 

    member this.IsDuration : bool = failwith "todo" 

    member this.IsFieldClass : bool = failwith "todo" 

    member this.IsFieldType : bool = failwith "todo" 

    member this.IsFieldRef : bool = failwith "todo" 

    member this.IsFile : bool = failwith "todo" 

    member this.IsGuid : bool = failwith "todo" 

    member this.IsInStream : bool = failwith "todo" 

    member this.IsInteger : bool = failwith "todo" 

    member this.IsKeyRef : bool = failwith "todo" 

    member this.IsOption : bool = failwith "todo" 

    member this.IsOutStream : bool = failwith "todo" 

    member this.IsRecord : bool = failwith "todo" 

    member this.IsRecordId : bool = failwith "todo" 

    member this.IsRecordRef : bool = failwith "todo" 

    member this.IsReport : bool = failwith "todo" 

    member this.IsTableFilter : bool = failwith "todo" 

    member this.IsTextConst : bool = failwith "todo" 

    member this.IsTime : bool = failwith "todo" 

    member this.IsTransactionType : bool = failwith "todo" 

    member this.IsExecutionMode : bool = failwith "todo" 

    member this.IsExecutionContext : bool = failwith "todo" 

    member this.IsTypeId : bool = failwith "todo" 

    member this.IsXmlPort : bool = failwith "todo" 

    member this.IsString : bool = failwith "todo" 

    member this.IsText : bool = failwith "todo" 

    member this.IsWideChar : bool = failwith "todo" 

    member this.IsChar : bool = failwith "todo" 

    member this.IsFilterPageBuilder : bool = failwith "todo" 

    member this.IsClientType : bool = failwith "todo" 

    member this.IsSecurityFiltering : bool = failwith "todo" 

    member this.IsObjectType : bool = failwith "todo" 

    member this.IsTextEncoding : bool = failwith "todo" 

    member this.IsReportFormat : bool = failwith "todo" 

    member this.IsDefaultLayout : bool = failwith "todo" 

    member this.IsTableConnectionType : bool = failwith "todo" 

    member this.IsNotification : bool = failwith "todo" 

    member this.IsNotificationScope : bool = failwith "todo" 

    member this.IsSessionSettings : bool = failwith "todo" 

    member this.IsTestPermissions : bool = failwith "todo" 

    member this.IsDataClassification : bool = failwith "todo" 

    member this.IsDataClassificationType : bool = failwith "todo" 

    member this.IsDataScope : bool = failwith "todo" 

    member this.IsVerbosity : bool = failwith "todo" 

    member this.IsTransactionModel : bool = failwith "todo" 

    member this.IsJsonArray : bool = failwith "todo" 

    member this.IsJsonObject : bool = failwith "todo" 

    member this.IsJsonToken : bool = failwith "todo" 

    member this.IsJsonValue : bool = failwith "todo" 

    member this.IsXmlAttribute : bool = failwith "todo" 

    member this.IsXmlAttributeCollection : bool = failwith "todo" 

    member this.IsXmlCData : bool = failwith "todo" 

    member this.IsXmlComment : bool = failwith "todo" 

    member this.IsXmlDeclaration : bool = failwith "todo" 

    member this.IsXmlDocument : bool = failwith "todo" 

    member this.IsXmlDocumentType : bool = failwith "todo" 

    member this.IsXmlElement : bool = failwith "todo" 

    member this.IsXmlNamespaceManager : bool = failwith "todo" 

    member this.IsXmlNameTable : bool = failwith "todo" 

    member this.IsXmlNode : bool = failwith "todo" 

    member this.IsXmlNodeList : bool = failwith "todo" 

    member this.IsXmlProcessingInstruction : bool = failwith "todo" 

    member this.IsXmlPortReference : bool = failwith "todo" 

    member this.IsXmlReadOptions : bool = failwith "todo" 

    member this.IsXmlText : bool = failwith "todo" 

    member this.IsXmlWriteOptions : bool = failwith "todo" 

    member this.IsTextBuilder : bool = failwith "todo" 


[<CompiledName("MediaSetSystemRecord")>]
type ALMediaSetSystemRecord() =
    inherit ALComplexValue()
    
    member this.SetRecFilter ()  : unit = failwith "todo" 

    member this.SetPermissionFilter ()  : unit = failwith "todo" 

    member this.ClearMarks ()  : unit = failwith "todo" 

    member this.Get (values:'t[]) : unit = failwith "todo" 

    member this.GetSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.GetBySystemId (systemId:Guid) : bool = failwith "todo" 

    member this.SetCurrentKey (fields:int[]) : bool = failwith "todo" 

    member this.SetLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AddLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AreFieldsLoaded (fields:int[]) : bool = failwith "todo" 

    member this.LoadFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcFields (fields:int[]) : bool = failwith "todo" 

    member this.SetAutoCalcFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcSums (fields:int[]) : bool = failwith "todo" 

    member this.ChangeCompany ()  : bool = failwith "todo" 

    member this.ChangeCompany (companyName:string) : bool = failwith "todo" 

    member this.Insert ()  : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool,insertWithSystemId:bool) : unit = failwith "todo" 

    member this.Delete ()  : unit = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool) : bool = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool,isBulkDelete:bool) : bool = failwith "todo" 

    member this.DeleteAll ()  : unit = failwith "todo" 

    member this.DeleteAll (runTrigger:bool) : unit = failwith "todo" 

    member this.Modify ()  : bool = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool,isBulkModify:bool) : bool = failwith "todo" 

    member this.Rename (values:'t[]) : unit = failwith "todo" 

    member this.RenameSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord,shareTableHandle:bool) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecordRef) : unit = failwith "todo" 

    member this.CopyFilters (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool,skipFieldsNotMatchingType:bool) : unit = failwith "todo" 

    member this.SetView (view:string) : unit = failwith "todo" 

    member this.DeleteLink (linkId:int) : unit = failwith "todo" 

    member this.DeleteLinks ()  : unit = failwith "todo" 

    member this.FieldExists (fieldNo:int) : bool = failwith "todo" 

    member this.AddLink (url:string) : int = failwith "todo" 

    member this.AddLink (url:string,description:string) : int = failwith "todo" 

    member this.Validate (fieldNo:int,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Validate (fieldNo:int,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Relation (fieldNo:int) : int = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetFilter (fieldNo:int,expression:string,values:'t[]) : unit = failwith "todo" 

    member this.GetFilter (fieldNo:int) : string = failwith "todo" 

    member this.GetRangeMin (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMinSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.GetRangeMax (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMaxSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.CopyFilter (fromField:int,toRecord:ALRecord,toField:int) : unit = failwith "todo" 

    member this.TestField (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldNotBlankOrZero (fieldNo:int) : bool = failwith "todo" 

    member this.TestFieldMustBeBlankOrZero (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValue (fieldNo:int,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValueSafe (fieldNo:int,expectedType:ALType,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorText:string) : unit = failwith "todo" 

    member this.FieldName (fieldNo:int) : string = failwith "todo" 

    member this.FieldCaption (fieldNo:int) : string = failwith "todo" 

    member this.FieldActive (fieldNo:int) : bool = failwith "todo" 

    member this.FieldNo (field:string) : int = failwith "todo" 

    member this.FieldNo (fieldNo:int) : int = failwith "todo" 

    member this.SetAscending (fieldNo:int,ascending:bool) : unit = failwith "todo" 

    member this.GetAscending (fieldNo:int) : bool = failwith "todo" 

    member this.Get (recordId:ALRecordId) : bool = failwith "todo" 

    member this.Mark ()  : bool = failwith "todo" 

    member this.Mark (markValue:bool) : bool = failwith "todo" 

    member this.Init ()  : unit = failwith "todo" 

    member this.Find ()  : unit = failwith "todo" 

    member this.Find (searchMethod:string) : unit = failwith "todo" 

    member this.FindFirst ()  : unit = failwith "todo" 

    member this.FindLast ()  : unit = failwith "todo" 

    member this.FindSet ()  : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool) : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool,updateKey:bool) : unit = failwith "todo" 

    member this.Next ()  : int = failwith "todo" 

    member this.Next (steps:int) : int = failwith "todo" 

    member this.Reset ()  : unit = failwith "todo" 

    member this.LockTable ()  : unit = failwith "todo" 

    member this.LockTable (wait:bool) : unit = failwith "todo" 

    member this.LockTable (wait:bool,versionCheck:bool) : unit = failwith "todo" 

    member this.Consistent (isConsistent:bool) : unit = failwith "todo" 

    member this.GetPosition ()  : string = failwith "todo" 

    member this.GetPosition (useCaptions:bool) : string = failwith "todo" 

    member this.SetPosition (position:string) : unit = failwith "todo" 

    member this.GetView ()  : string = failwith "todo" 

    member this.GetView (useCaptions:bool) : string = failwith "todo" 

    member this.HasLinks : bool = failwith "todo" 

    member this.TableName : string = failwith "todo" 

    member this.TableCaption : string = failwith "todo" 

    member this.Ascending : bool = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.CountApprox : int = failwith "todo" 

    member this.IsEmpty : bool = failwith "todo" 

    member this.IsDirty : bool = failwith "todo" 

    member this.CurrentKeyIndex : int = failwith "todo" 

    member this.RecordId : ALRecordId = failwith "todo" 

    member this.GetFilters : string = failwith "todo" 

    member this.CurrentKey : string = failwith "todo" 

    member this.ReadPermission : bool = failwith "todo" 

    member this.WritePermission : bool = failwith "todo" 

    member this.ReadConsistency : bool = failwith "todo" 

    member this.RecordLevelLocking : bool = failwith "todo" 

    member this.FilterGroup : int = failwith "todo" 

    member this.MarkedOnly : bool = failwith "todo" 

    member this.KeyCount : int = failwith "todo" 

    member this.FieldCount : int = failwith "todo" 

    member this.HasFilter : bool = failwith "todo" 

    member this.IsTemporary : bool = failwith "todo" 

    member this.CurrentCompany : string = failwith "todo" 


[<CompiledName("MediaSystemRecord")>]
type ALMediaSystemRecord() =
    inherit ALComplexValue()
    
    member this.SetRecFilter ()  : unit = failwith "todo" 

    member this.SetPermissionFilter ()  : unit = failwith "todo" 

    member this.ClearMarks ()  : unit = failwith "todo" 

    member this.Get (values:'t[]) : unit = failwith "todo" 

    member this.GetSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.GetBySystemId (systemId:Guid) : bool = failwith "todo" 

    member this.SetCurrentKey (fields:int[]) : bool = failwith "todo" 

    member this.SetLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AddLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AreFieldsLoaded (fields:int[]) : bool = failwith "todo" 

    member this.LoadFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcFields (fields:int[]) : bool = failwith "todo" 

    member this.SetAutoCalcFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcSums (fields:int[]) : bool = failwith "todo" 

    member this.ChangeCompany ()  : bool = failwith "todo" 

    member this.ChangeCompany (companyName:string) : bool = failwith "todo" 

    member this.Insert ()  : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool,insertWithSystemId:bool) : unit = failwith "todo" 

    member this.Delete ()  : unit = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool) : bool = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool,isBulkDelete:bool) : bool = failwith "todo" 

    member this.DeleteAll ()  : unit = failwith "todo" 

    member this.DeleteAll (runTrigger:bool) : unit = failwith "todo" 

    member this.Modify ()  : bool = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool,isBulkModify:bool) : bool = failwith "todo" 

    member this.Rename (values:'t[]) : unit = failwith "todo" 

    member this.RenameSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord,shareTableHandle:bool) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecordRef) : unit = failwith "todo" 

    member this.CopyFilters (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool,skipFieldsNotMatchingType:bool) : unit = failwith "todo" 

    member this.SetView (view:string) : unit = failwith "todo" 

    member this.DeleteLink (linkId:int) : unit = failwith "todo" 

    member this.DeleteLinks ()  : unit = failwith "todo" 

    member this.FieldExists (fieldNo:int) : bool = failwith "todo" 

    member this.AddLink (url:string) : int = failwith "todo" 

    member this.AddLink (url:string,description:string) : int = failwith "todo" 

    member this.Validate (fieldNo:int,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Validate (fieldNo:int,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Relation (fieldNo:int) : int = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetFilter (fieldNo:int,expression:string,values:'t[]) : unit = failwith "todo" 

    member this.GetFilter (fieldNo:int) : string = failwith "todo" 

    member this.GetRangeMin (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMinSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.GetRangeMax (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMaxSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.CopyFilter (fromField:int,toRecord:ALRecord,toField:int) : unit = failwith "todo" 

    member this.TestField (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldNotBlankOrZero (fieldNo:int) : bool = failwith "todo" 

    member this.TestFieldMustBeBlankOrZero (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValue (fieldNo:int,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValueSafe (fieldNo:int,expectedType:ALType,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorText:string) : unit = failwith "todo" 

    member this.FieldName (fieldNo:int) : string = failwith "todo" 

    member this.FieldCaption (fieldNo:int) : string = failwith "todo" 

    member this.FieldActive (fieldNo:int) : bool = failwith "todo" 

    member this.FieldNo (field:string) : int = failwith "todo" 

    member this.FieldNo (fieldNo:int) : int = failwith "todo" 

    member this.SetAscending (fieldNo:int,ascending:bool) : unit = failwith "todo" 

    member this.GetAscending (fieldNo:int) : bool = failwith "todo" 

    member this.Get (recordId:ALRecordId) : bool = failwith "todo" 

    member this.Mark ()  : bool = failwith "todo" 

    member this.Mark (markValue:bool) : bool = failwith "todo" 

    member this.Init ()  : unit = failwith "todo" 

    member this.Find ()  : unit = failwith "todo" 

    member this.Find (searchMethod:string) : unit = failwith "todo" 

    member this.FindFirst ()  : unit = failwith "todo" 

    member this.FindLast ()  : unit = failwith "todo" 

    member this.FindSet ()  : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool) : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool,updateKey:bool) : unit = failwith "todo" 

    member this.Next ()  : int = failwith "todo" 

    member this.Next (steps:int) : int = failwith "todo" 

    member this.Reset ()  : unit = failwith "todo" 

    member this.LockTable ()  : unit = failwith "todo" 

    member this.LockTable (wait:bool) : unit = failwith "todo" 

    member this.LockTable (wait:bool,versionCheck:bool) : unit = failwith "todo" 

    member this.Consistent (isConsistent:bool) : unit = failwith "todo" 

    member this.GetPosition ()  : string = failwith "todo" 

    member this.GetPosition (useCaptions:bool) : string = failwith "todo" 

    member this.SetPosition (position:string) : unit = failwith "todo" 

    member this.GetView ()  : string = failwith "todo" 

    member this.GetView (useCaptions:bool) : string = failwith "todo" 

    member this.HasLinks : bool = failwith "todo" 

    member this.TableName : string = failwith "todo" 

    member this.TableCaption : string = failwith "todo" 

    member this.Ascending : bool = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.CountApprox : int = failwith "todo" 

    member this.IsEmpty : bool = failwith "todo" 

    member this.IsDirty : bool = failwith "todo" 

    member this.CurrentKeyIndex : int = failwith "todo" 

    member this.RecordId : ALRecordId = failwith "todo" 

    member this.GetFilters : string = failwith "todo" 

    member this.CurrentKey : string = failwith "todo" 

    member this.ReadPermission : bool = failwith "todo" 

    member this.WritePermission : bool = failwith "todo" 

    member this.ReadConsistency : bool = failwith "todo" 

    member this.RecordLevelLocking : bool = failwith "todo" 

    member this.FilterGroup : int = failwith "todo" 

    member this.MarkedOnly : bool = failwith "todo" 

    member this.KeyCount : int = failwith "todo" 

    member this.FieldCount : int = failwith "todo" 

    member this.HasFilter : bool = failwith "todo" 

    member this.IsTemporary : bool = failwith "todo" 

    member this.CurrentCompany : string = failwith "todo" 


[<CompiledName("MediaThumbnailsSystemRecord")>]
type ALMediaThumbnailsSystemRecord() =
    inherit ALComplexValue()
    
    member this.SetRecFilter ()  : unit = failwith "todo" 

    member this.SetPermissionFilter ()  : unit = failwith "todo" 

    member this.ClearMarks ()  : unit = failwith "todo" 

    member this.Get (values:'t[]) : unit = failwith "todo" 

    member this.GetSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.GetBySystemId (systemId:Guid) : bool = failwith "todo" 

    member this.SetCurrentKey (fields:int[]) : bool = failwith "todo" 

    member this.SetLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AddLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AreFieldsLoaded (fields:int[]) : bool = failwith "todo" 

    member this.LoadFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcFields (fields:int[]) : bool = failwith "todo" 

    member this.SetAutoCalcFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcSums (fields:int[]) : bool = failwith "todo" 

    member this.ChangeCompany ()  : bool = failwith "todo" 

    member this.ChangeCompany (companyName:string) : bool = failwith "todo" 

    member this.Insert ()  : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool,insertWithSystemId:bool) : unit = failwith "todo" 

    member this.Delete ()  : unit = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool) : bool = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool,isBulkDelete:bool) : bool = failwith "todo" 

    member this.DeleteAll ()  : unit = failwith "todo" 

    member this.DeleteAll (runTrigger:bool) : unit = failwith "todo" 

    member this.Modify ()  : bool = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool,isBulkModify:bool) : bool = failwith "todo" 

    member this.Rename (values:'t[]) : unit = failwith "todo" 

    member this.RenameSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord,shareTableHandle:bool) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecordRef) : unit = failwith "todo" 

    member this.CopyFilters (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool,skipFieldsNotMatchingType:bool) : unit = failwith "todo" 

    member this.SetView (view:string) : unit = failwith "todo" 

    member this.DeleteLink (linkId:int) : unit = failwith "todo" 

    member this.DeleteLinks ()  : unit = failwith "todo" 

    member this.FieldExists (fieldNo:int) : bool = failwith "todo" 

    member this.AddLink (url:string) : int = failwith "todo" 

    member this.AddLink (url:string,description:string) : int = failwith "todo" 

    member this.Validate (fieldNo:int,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Validate (fieldNo:int,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Relation (fieldNo:int) : int = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetFilter (fieldNo:int,expression:string,values:'t[]) : unit = failwith "todo" 

    member this.GetFilter (fieldNo:int) : string = failwith "todo" 

    member this.GetRangeMin (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMinSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.GetRangeMax (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMaxSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.CopyFilter (fromField:int,toRecord:ALRecord,toField:int) : unit = failwith "todo" 

    member this.TestField (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldNotBlankOrZero (fieldNo:int) : bool = failwith "todo" 

    member this.TestFieldMustBeBlankOrZero (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValue (fieldNo:int,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValueSafe (fieldNo:int,expectedType:ALType,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorText:string) : unit = failwith "todo" 

    member this.FieldName (fieldNo:int) : string = failwith "todo" 

    member this.FieldCaption (fieldNo:int) : string = failwith "todo" 

    member this.FieldActive (fieldNo:int) : bool = failwith "todo" 

    member this.FieldNo (field:string) : int = failwith "todo" 

    member this.FieldNo (fieldNo:int) : int = failwith "todo" 

    member this.SetAscending (fieldNo:int,ascending:bool) : unit = failwith "todo" 

    member this.GetAscending (fieldNo:int) : bool = failwith "todo" 

    member this.Get (recordId:ALRecordId) : bool = failwith "todo" 

    member this.Mark ()  : bool = failwith "todo" 

    member this.Mark (markValue:bool) : bool = failwith "todo" 

    member this.Init ()  : unit = failwith "todo" 

    member this.Find ()  : unit = failwith "todo" 

    member this.Find (searchMethod:string) : unit = failwith "todo" 

    member this.FindFirst ()  : unit = failwith "todo" 

    member this.FindLast ()  : unit = failwith "todo" 

    member this.FindSet ()  : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool) : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool,updateKey:bool) : unit = failwith "todo" 

    member this.Next ()  : int = failwith "todo" 

    member this.Next (steps:int) : int = failwith "todo" 

    member this.Reset ()  : unit = failwith "todo" 

    member this.LockTable ()  : unit = failwith "todo" 

    member this.LockTable (wait:bool) : unit = failwith "todo" 

    member this.LockTable (wait:bool,versionCheck:bool) : unit = failwith "todo" 

    member this.Consistent (isConsistent:bool) : unit = failwith "todo" 

    member this.GetPosition ()  : string = failwith "todo" 

    member this.GetPosition (useCaptions:bool) : string = failwith "todo" 

    member this.SetPosition (position:string) : unit = failwith "todo" 

    member this.GetView ()  : string = failwith "todo" 

    member this.GetView (useCaptions:bool) : string = failwith "todo" 

    member this.HasLinks : bool = failwith "todo" 

    member this.TableName : string = failwith "todo" 

    member this.TableCaption : string = failwith "todo" 

    member this.Ascending : bool = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.CountApprox : int = failwith "todo" 

    member this.IsEmpty : bool = failwith "todo" 

    member this.IsDirty : bool = failwith "todo" 

    member this.CurrentKeyIndex : int = failwith "todo" 

    member this.RecordId : ALRecordId = failwith "todo" 

    member this.GetFilters : string = failwith "todo" 

    member this.CurrentKey : string = failwith "todo" 

    member this.ReadPermission : bool = failwith "todo" 

    member this.WritePermission : bool = failwith "todo" 

    member this.ReadConsistency : bool = failwith "todo" 

    member this.RecordLevelLocking : bool = failwith "todo" 

    member this.FilterGroup : int = failwith "todo" 

    member this.MarkedOnly : bool = failwith "todo" 

    member this.KeyCount : int = failwith "todo" 

    member this.FieldCount : int = failwith "todo" 

    member this.HasFilter : bool = failwith "todo" 

    member this.IsTemporary : bool = failwith "todo" 

    member this.CurrentCompany : string = failwith "todo" 


[<CompiledName("CodeunitHandle")>]
type ALCodeunitHandle() =
    inherit ALComplexValue()
    
    member this.Assign (other:ALCodeunitHandle) : unit = failwith "todo" 


[<CompiledName("DebuggerBreakpointSystemRecord")>]
type ALDebuggerBreakpointSystemRecord() =
    inherit ALComplexValue()
    
    member this.SetRecFilter ()  : unit = failwith "todo" 

    member this.SetPermissionFilter ()  : unit = failwith "todo" 

    member this.ClearMarks ()  : unit = failwith "todo" 

    member this.Get (values:'t[]) : unit = failwith "todo" 

    member this.GetSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.GetBySystemId (systemId:Guid) : bool = failwith "todo" 

    member this.SetCurrentKey (fields:int[]) : bool = failwith "todo" 

    member this.SetLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AddLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AreFieldsLoaded (fields:int[]) : bool = failwith "todo" 

    member this.LoadFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcFields (fields:int[]) : bool = failwith "todo" 

    member this.SetAutoCalcFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcSums (fields:int[]) : bool = failwith "todo" 

    member this.ChangeCompany ()  : bool = failwith "todo" 

    member this.ChangeCompany (companyName:string) : bool = failwith "todo" 

    member this.Insert ()  : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool,insertWithSystemId:bool) : unit = failwith "todo" 

    member this.Delete ()  : unit = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool) : bool = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool,isBulkDelete:bool) : bool = failwith "todo" 

    member this.DeleteAll ()  : unit = failwith "todo" 

    member this.DeleteAll (runTrigger:bool) : unit = failwith "todo" 

    member this.Modify ()  : bool = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool,isBulkModify:bool) : bool = failwith "todo" 

    member this.Rename (values:'t[]) : unit = failwith "todo" 

    member this.RenameSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord,shareTableHandle:bool) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecordRef) : unit = failwith "todo" 

    member this.CopyFilters (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool,skipFieldsNotMatchingType:bool) : unit = failwith "todo" 

    member this.SetView (view:string) : unit = failwith "todo" 

    member this.DeleteLink (linkId:int) : unit = failwith "todo" 

    member this.DeleteLinks ()  : unit = failwith "todo" 

    member this.FieldExists (fieldNo:int) : bool = failwith "todo" 

    member this.AddLink (url:string) : int = failwith "todo" 

    member this.AddLink (url:string,description:string) : int = failwith "todo" 

    member this.Validate (fieldNo:int,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Validate (fieldNo:int,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Relation (fieldNo:int) : int = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetFilter (fieldNo:int,expression:string,values:'t[]) : unit = failwith "todo" 

    member this.GetFilter (fieldNo:int) : string = failwith "todo" 

    member this.GetRangeMin (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMinSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.GetRangeMax (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMaxSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.CopyFilter (fromField:int,toRecord:ALRecord,toField:int) : unit = failwith "todo" 

    member this.TestField (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldNotBlankOrZero (fieldNo:int) : bool = failwith "todo" 

    member this.TestFieldMustBeBlankOrZero (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValue (fieldNo:int,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValueSafe (fieldNo:int,expectedType:ALType,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorText:string) : unit = failwith "todo" 

    member this.FieldName (fieldNo:int) : string = failwith "todo" 

    member this.FieldCaption (fieldNo:int) : string = failwith "todo" 

    member this.FieldActive (fieldNo:int) : bool = failwith "todo" 

    member this.FieldNo (field:string) : int = failwith "todo" 

    member this.FieldNo (fieldNo:int) : int = failwith "todo" 

    member this.SetAscending (fieldNo:int,ascending:bool) : unit = failwith "todo" 

    member this.GetAscending (fieldNo:int) : bool = failwith "todo" 

    member this.Get (recordId:ALRecordId) : bool = failwith "todo" 

    member this.Mark ()  : bool = failwith "todo" 

    member this.Mark (markValue:bool) : bool = failwith "todo" 

    member this.Init ()  : unit = failwith "todo" 

    member this.Find ()  : unit = failwith "todo" 

    member this.Find (searchMethod:string) : unit = failwith "todo" 

    member this.FindFirst ()  : unit = failwith "todo" 

    member this.FindLast ()  : unit = failwith "todo" 

    member this.FindSet ()  : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool) : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool,updateKey:bool) : unit = failwith "todo" 

    member this.Next ()  : int = failwith "todo" 

    member this.Next (steps:int) : int = failwith "todo" 

    member this.Reset ()  : unit = failwith "todo" 

    member this.LockTable ()  : unit = failwith "todo" 

    member this.LockTable (wait:bool) : unit = failwith "todo" 

    member this.LockTable (wait:bool,versionCheck:bool) : unit = failwith "todo" 

    member this.Consistent (isConsistent:bool) : unit = failwith "todo" 

    member this.GetPosition ()  : string = failwith "todo" 

    member this.GetPosition (useCaptions:bool) : string = failwith "todo" 

    member this.SetPosition (position:string) : unit = failwith "todo" 

    member this.GetView ()  : string = failwith "todo" 

    member this.GetView (useCaptions:bool) : string = failwith "todo" 

    member this.HasLinks : bool = failwith "todo" 

    member this.TableName : string = failwith "todo" 

    member this.TableCaption : string = failwith "todo" 

    member this.Ascending : bool = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.CountApprox : int = failwith "todo" 

    member this.IsEmpty : bool = failwith "todo" 

    member this.IsDirty : bool = failwith "todo" 

    member this.CurrentKeyIndex : int = failwith "todo" 

    member this.RecordId : ALRecordId = failwith "todo" 

    member this.GetFilters : string = failwith "todo" 

    member this.CurrentKey : string = failwith "todo" 

    member this.ReadPermission : bool = failwith "todo" 

    member this.WritePermission : bool = failwith "todo" 

    member this.ReadConsistency : bool = failwith "todo" 

    member this.RecordLevelLocking : bool = failwith "todo" 

    member this.FilterGroup : int = failwith "todo" 

    member this.MarkedOnly : bool = failwith "todo" 

    member this.KeyCount : int = failwith "todo" 

    member this.FieldCount : int = failwith "todo" 

    member this.HasFilter : bool = failwith "todo" 

    member this.IsTemporary : bool = failwith "todo" 

    member this.CurrentCompany : string = failwith "todo" 


[<CompiledName("DebuggerWatchSystemRecord")>]
type ALDebuggerWatchSystemRecord() =
    inherit ALComplexValue()
    
    member this.SetRecFilter ()  : unit = failwith "todo" 

    member this.SetPermissionFilter ()  : unit = failwith "todo" 

    member this.ClearMarks ()  : unit = failwith "todo" 

    member this.Get (values:'t[]) : unit = failwith "todo" 

    member this.GetSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.GetBySystemId (systemId:Guid) : bool = failwith "todo" 

    member this.SetCurrentKey (fields:int[]) : bool = failwith "todo" 

    member this.SetLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AddLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AreFieldsLoaded (fields:int[]) : bool = failwith "todo" 

    member this.LoadFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcFields (fields:int[]) : bool = failwith "todo" 

    member this.SetAutoCalcFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcSums (fields:int[]) : bool = failwith "todo" 

    member this.ChangeCompany ()  : bool = failwith "todo" 

    member this.ChangeCompany (companyName:string) : bool = failwith "todo" 

    member this.Insert ()  : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool,insertWithSystemId:bool) : unit = failwith "todo" 

    member this.Delete ()  : unit = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool) : bool = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool,isBulkDelete:bool) : bool = failwith "todo" 

    member this.DeleteAll ()  : unit = failwith "todo" 

    member this.DeleteAll (runTrigger:bool) : unit = failwith "todo" 

    member this.Modify ()  : bool = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool,isBulkModify:bool) : bool = failwith "todo" 

    member this.Rename (values:'t[]) : unit = failwith "todo" 

    member this.RenameSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord,shareTableHandle:bool) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecordRef) : unit = failwith "todo" 

    member this.CopyFilters (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool,skipFieldsNotMatchingType:bool) : unit = failwith "todo" 

    member this.SetView (view:string) : unit = failwith "todo" 

    member this.DeleteLink (linkId:int) : unit = failwith "todo" 

    member this.DeleteLinks ()  : unit = failwith "todo" 

    member this.FieldExists (fieldNo:int) : bool = failwith "todo" 

    member this.AddLink (url:string) : int = failwith "todo" 

    member this.AddLink (url:string,description:string) : int = failwith "todo" 

    member this.Validate (fieldNo:int,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Validate (fieldNo:int,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Relation (fieldNo:int) : int = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetFilter (fieldNo:int,expression:string,values:'t[]) : unit = failwith "todo" 

    member this.GetFilter (fieldNo:int) : string = failwith "todo" 

    member this.GetRangeMin (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMinSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.GetRangeMax (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMaxSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.CopyFilter (fromField:int,toRecord:ALRecord,toField:int) : unit = failwith "todo" 

    member this.TestField (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldNotBlankOrZero (fieldNo:int) : bool = failwith "todo" 

    member this.TestFieldMustBeBlankOrZero (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValue (fieldNo:int,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValueSafe (fieldNo:int,expectedType:ALType,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorText:string) : unit = failwith "todo" 

    member this.FieldName (fieldNo:int) : string = failwith "todo" 

    member this.FieldCaption (fieldNo:int) : string = failwith "todo" 

    member this.FieldActive (fieldNo:int) : bool = failwith "todo" 

    member this.FieldNo (field:string) : int = failwith "todo" 

    member this.FieldNo (fieldNo:int) : int = failwith "todo" 

    member this.SetAscending (fieldNo:int,ascending:bool) : unit = failwith "todo" 

    member this.GetAscending (fieldNo:int) : bool = failwith "todo" 

    member this.Get (recordId:ALRecordId) : bool = failwith "todo" 

    member this.Mark ()  : bool = failwith "todo" 

    member this.Mark (markValue:bool) : bool = failwith "todo" 

    member this.Init ()  : unit = failwith "todo" 

    member this.Find ()  : unit = failwith "todo" 

    member this.Find (searchMethod:string) : unit = failwith "todo" 

    member this.FindFirst ()  : unit = failwith "todo" 

    member this.FindLast ()  : unit = failwith "todo" 

    member this.FindSet ()  : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool) : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool,updateKey:bool) : unit = failwith "todo" 

    member this.Next ()  : int = failwith "todo" 

    member this.Next (steps:int) : int = failwith "todo" 

    member this.Reset ()  : unit = failwith "todo" 

    member this.LockTable ()  : unit = failwith "todo" 

    member this.LockTable (wait:bool) : unit = failwith "todo" 

    member this.LockTable (wait:bool,versionCheck:bool) : unit = failwith "todo" 

    member this.Consistent (isConsistent:bool) : unit = failwith "todo" 

    member this.GetPosition ()  : string = failwith "todo" 

    member this.GetPosition (useCaptions:bool) : string = failwith "todo" 

    member this.SetPosition (position:string) : unit = failwith "todo" 

    member this.GetView ()  : string = failwith "todo" 

    member this.GetView (useCaptions:bool) : string = failwith "todo" 

    member this.HasLinks : bool = failwith "todo" 

    member this.TableName : string = failwith "todo" 

    member this.TableCaption : string = failwith "todo" 

    member this.Ascending : bool = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.CountApprox : int = failwith "todo" 

    member this.IsEmpty : bool = failwith "todo" 

    member this.IsDirty : bool = failwith "todo" 

    member this.CurrentKeyIndex : int = failwith "todo" 

    member this.RecordId : ALRecordId = failwith "todo" 

    member this.GetFilters : string = failwith "todo" 

    member this.CurrentKey : string = failwith "todo" 

    member this.ReadPermission : bool = failwith "todo" 

    member this.WritePermission : bool = failwith "todo" 

    member this.ReadConsistency : bool = failwith "todo" 

    member this.RecordLevelLocking : bool = failwith "todo" 

    member this.FilterGroup : int = failwith "todo" 

    member this.MarkedOnly : bool = failwith "todo" 

    member this.KeyCount : int = failwith "todo" 

    member this.FieldCount : int = failwith "todo" 

    member this.HasFilter : bool = failwith "todo" 

    member this.IsTemporary : bool = failwith "todo" 

    member this.CurrentCompany : string = failwith "todo" 


[<CompiledName("DocumentServiceSystemRecord")>]
type ALDocumentServiceSystemRecord() =
    inherit ALComplexValue()
    
    member this.SetRecFilter ()  : unit = failwith "todo" 

    member this.SetPermissionFilter ()  : unit = failwith "todo" 

    member this.ClearMarks ()  : unit = failwith "todo" 

    member this.Get (values:'t[]) : unit = failwith "todo" 

    member this.GetSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.GetBySystemId (systemId:Guid) : bool = failwith "todo" 

    member this.SetCurrentKey (fields:int[]) : bool = failwith "todo" 

    member this.SetLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AddLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AreFieldsLoaded (fields:int[]) : bool = failwith "todo" 

    member this.LoadFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcFields (fields:int[]) : bool = failwith "todo" 

    member this.SetAutoCalcFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcSums (fields:int[]) : bool = failwith "todo" 

    member this.ChangeCompany ()  : bool = failwith "todo" 

    member this.ChangeCompany (companyName:string) : bool = failwith "todo" 

    member this.Insert ()  : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool,insertWithSystemId:bool) : unit = failwith "todo" 

    member this.Delete ()  : unit = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool) : bool = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool,isBulkDelete:bool) : bool = failwith "todo" 

    member this.DeleteAll ()  : unit = failwith "todo" 

    member this.DeleteAll (runTrigger:bool) : unit = failwith "todo" 

    member this.Modify ()  : bool = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool,isBulkModify:bool) : bool = failwith "todo" 

    member this.Rename (values:'t[]) : unit = failwith "todo" 

    member this.RenameSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord,shareTableHandle:bool) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecordRef) : unit = failwith "todo" 

    member this.CopyFilters (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool,skipFieldsNotMatchingType:bool) : unit = failwith "todo" 

    member this.SetView (view:string) : unit = failwith "todo" 

    member this.DeleteLink (linkId:int) : unit = failwith "todo" 

    member this.DeleteLinks ()  : unit = failwith "todo" 

    member this.FieldExists (fieldNo:int) : bool = failwith "todo" 

    member this.AddLink (url:string) : int = failwith "todo" 

    member this.AddLink (url:string,description:string) : int = failwith "todo" 

    member this.Validate (fieldNo:int,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Validate (fieldNo:int,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.ValidateSafe (fieldNo:int,expectedType:ALType,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Relation (fieldNo:int) : int = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAll (fieldNo:int,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAllSafe (fieldNo:int,expectedType:ALType,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t) : unit = failwith "todo" 

    member this.SetRange (fieldNo:int,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t) : unit = failwith "todo" 

    member this.SetRangeSafe (fieldNo:int,expectedType:ALType,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetFilter (fieldNo:int,expression:string,values:'t[]) : unit = failwith "todo" 

    member this.GetFilter (fieldNo:int) : string = failwith "todo" 

    member this.GetRangeMin (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMinSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.GetRangeMax (fieldNo:int) : 't = failwith "todo" 

    member this.GetRangeMaxSafe (fieldNo:int,expectedType:ALType) : 't = failwith "todo" 

    member this.CopyFilter (fromField:int,toRecord:ALRecord,toField:int) : unit = failwith "todo" 

    member this.TestField (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldNotBlankOrZero (fieldNo:int) : bool = failwith "todo" 

    member this.TestFieldMustBeBlankOrZero (fieldNo:int) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (fieldNo:int,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (fieldNo:int,expectedType:ALType,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValue (fieldNo:int,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValueSafe (fieldNo:int,expectedType:ALType,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (fieldNo:int,errorText:string) : unit = failwith "todo" 

    member this.FieldName (fieldNo:int) : string = failwith "todo" 

    member this.FieldCaption (fieldNo:int) : string = failwith "todo" 

    member this.FieldActive (fieldNo:int) : bool = failwith "todo" 

    member this.FieldNo (field:string) : int = failwith "todo" 

    member this.FieldNo (fieldNo:int) : int = failwith "todo" 

    member this.SetAscending (fieldNo:int,ascending:bool) : unit = failwith "todo" 

    member this.GetAscending (fieldNo:int) : bool = failwith "todo" 

    member this.Get (recordId:ALRecordId) : bool = failwith "todo" 

    member this.Mark ()  : bool = failwith "todo" 

    member this.Mark (markValue:bool) : bool = failwith "todo" 

    member this.Init ()  : unit = failwith "todo" 

    member this.Find ()  : unit = failwith "todo" 

    member this.Find (searchMethod:string) : unit = failwith "todo" 

    member this.FindFirst ()  : unit = failwith "todo" 

    member this.FindLast ()  : unit = failwith "todo" 

    member this.FindSet ()  : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool) : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool,updateKey:bool) : unit = failwith "todo" 

    member this.Next ()  : int = failwith "todo" 

    member this.Next (steps:int) : int = failwith "todo" 

    member this.Reset ()  : unit = failwith "todo" 

    member this.LockTable ()  : unit = failwith "todo" 

    member this.LockTable (wait:bool) : unit = failwith "todo" 

    member this.LockTable (wait:bool,versionCheck:bool) : unit = failwith "todo" 

    member this.Consistent (isConsistent:bool) : unit = failwith "todo" 

    member this.GetPosition ()  : string = failwith "todo" 

    member this.GetPosition (useCaptions:bool) : string = failwith "todo" 

    member this.SetPosition (position:string) : unit = failwith "todo" 

    member this.GetView ()  : string = failwith "todo" 

    member this.GetView (useCaptions:bool) : string = failwith "todo" 

    member this.HasLinks : bool = failwith "todo" 

    member this.TableName : string = failwith "todo" 

    member this.TableCaption : string = failwith "todo" 

    member this.Ascending : bool = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.CountApprox : int = failwith "todo" 

    member this.IsEmpty : bool = failwith "todo" 

    member this.IsDirty : bool = failwith "todo" 

    member this.CurrentKeyIndex : int = failwith "todo" 

    member this.RecordId : ALRecordId = failwith "todo" 

    member this.GetFilters : string = failwith "todo" 

    member this.CurrentKey : string = failwith "todo" 

    member this.ReadPermission : bool = failwith "todo" 

    member this.WritePermission : bool = failwith "todo" 

    member this.ReadConsistency : bool = failwith "todo" 

    member this.RecordLevelLocking : bool = failwith "todo" 

    member this.FilterGroup : int = failwith "todo" 

    member this.MarkedOnly : bool = failwith "todo" 

    member this.KeyCount : int = failwith "todo" 

    member this.FieldCount : int = failwith "todo" 

    member this.HasFilter : bool = failwith "todo" 

    member this.IsTemporary : bool = failwith "todo" 

    member this.CurrentCompany : string = failwith "todo" 


[<CompiledName("InterfaceHandle")>]
type ALInterfaceHandle() =
    inherit ALComplexValue()
    
    member this.Assign (other:ALInterfaceHandle) : unit = failwith "todo" 


[<CompiledName("QueryHandle")>]
type ALQueryHandle() =
    inherit ALComplexValue()
    
    member this.Assign (other:ALQueryHandle) : unit = failwith "todo" 


[<CompiledName("ReportHandle")>]
type ALReportHandle() =
    inherit ALComplexValue()
    
    member this.Assign (other:ALReportHandle) : unit = failwith "todo" 


[<CompiledName("XmlPortHandle")>]
type ALXmlPortHandle() =
    inherit ALComplexValue()
    
    member this.Assign (other:ALXmlPortHandle) : unit = failwith "todo" 


[<CompiledName("TestPageHandle")>]
type ALTestPageHandle() =
    inherit ALComplexValue()
    
    member this.Assign (other:ALTestPageHandle) : unit = failwith "todo" 




[<AbstractClass>]
type ALRecord() =
    inherit ALObjectValue()
    member this.SetRecFilter ()  : unit = failwith "todo" 

    member this.SetPermissionFilter ()  : unit = failwith "todo" 

    member this.ClearMarks ()  : unit = failwith "todo" 

    member this.Get (values:obj[]) : unit = failwith "todo" 

    member this.GetSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.GetBySystemId (systemId:Guid) : bool = failwith "todo" 

    member this.SetCurrentKey (fields:int[]) : bool = failwith "todo" 

    member this.SetLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AddLoadFields (fields:int[]) : bool = failwith "todo" 

    member this.AreFieldsLoaded (fields:int[]) : bool = failwith "todo" 

    member this.LoadFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcFields (fields:int[]) : bool = failwith "todo" 

    member this.SetAutoCalcFields (fields:int[]) : bool = failwith "todo" 

    member this.CalcSums (fields:int[]) : bool = failwith "todo" 

    member this.ChangeCompany ()  : bool = failwith "todo" 

    member this.ChangeCompany (companyName:string) : bool = failwith "todo" 

    member this.Insert ()  : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Insert (runApplicationTrigger:bool,insertWithSystemId:bool) : unit = failwith "todo" 

    member this.Delete ()  : unit = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool) : bool = failwith "todo" 

    member this.Delete (runApplicationTrigger:bool,isBulkDelete:bool) : bool = failwith "todo" 

    member this.DeleteAll ()  : unit = failwith "todo" 

    member this.DeleteAll (runTrigger:bool) : unit = failwith "todo" 

    member this.Modify ()  : bool = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool) : unit = failwith "todo" 

    member this.Modify (runApplicationTrigger:bool,isBulkModify:bool) : bool = failwith "todo" 

    member this.Rename (values:'t[]) : unit = failwith "todo" 

    member this.RenameSafe (compilerHashCode:int,values:'t[]) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.Copy (fromRecord:ALRecord,shareTableHandle:bool) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.CopyLinks (fromRecord:ALRecordRef) : unit = failwith "todo" 

    member this.CopyFilters (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool) : unit = failwith "todo" 

    member this.TransferFields (fromRecord:ALRecord,initPrimaryKeyFields:bool,skipFieldsNotMatchingType:bool) : unit = failwith "todo" 

    member this.SetView (view:string) : unit = failwith "todo" 

    member this.DeleteLink (linkId:int) : unit = failwith "todo" 

    member this.DeleteLinks ()  : unit = failwith "todo" 

    member this.FieldExists (fieldNo:int) : bool = failwith "todo" 

    member this.AddLink (url:string) : int = failwith "todo" 

    member this.AddLink (url:string,description:string) : int = failwith "todo" 

    member this.Validate (field:string,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Validate (field:string,newValue:'t,callerRecord:ALRecord) : unit = failwith "todo" 

    member this.Relation (field:string) : int = failwith "todo" 

    member this.ModifyAll (field:string,newValue:'t) : unit = failwith "todo" 

    member this.ModifyAll (field:string,newValue:'t,runTrigger:bool) : unit = failwith "todo" 

    member this.SetRange (field:string) : unit = failwith "todo" 

    member this.SetRange (field:string,fromValue:'t) : unit = failwith "todo" 

    member this.SetRange (field:string,fromValue:'t,toValue:'t) : unit = failwith "todo" 

    member this.SetFilter (field:string,expression:string,values:'t[]) : unit = failwith "todo" 

    member this.GetFilter (field:string) : string = failwith "todo" 

    member this.GetRangeMin (field:string) : 't = failwith "todo" 

    member this.GetRangeMinSafe (field:string,expectedType:ALType) : 't = failwith "todo" 

    member this.GetRangeMax (field:string) : 't = failwith "todo" 

    member this.GetRangeMaxSafe (field:string,expectedType:ALType) : 't = failwith "todo" 

    member this.CopyFilter (fromField:int,toRecord:ALRecord,toField:int) : unit = failwith "todo" 

    member this.TestField (field:string) : unit = failwith "todo" 

    member this.TestFieldNotBlankOrZero (field:string) : bool = failwith "todo" 

    member this.TestFieldMustBeBlankOrZero (field:string) : unit = failwith "todo" 

    member this.TestFieldSafe (field:string,expectedType:ALType,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (field:string,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (field:string,expectedType:ALType,testValue:byte,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (field:string,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (field:string,expectedType:ALType,testValue:bool,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (field:string,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (field:string,expectedType:ALType,testValue:int,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (field:string,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (field:string,expectedType:ALType,testValue:decimal,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (field:string,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (field:string,expectedType:ALType,testValue:Guid,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestField (field:string,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldSafe (field:string,expectedType:ALType,testValue:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValue (field:string,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.TestFieldNavValueSafe (field:string,expectedType:ALType,testValue:'t,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (field:string,errorInfo:ALALErrorInfo) : unit = failwith "todo" 

    member this.FieldError (field:string,errorText:string) : unit = failwith "todo" 

    member this.FieldName (field:string) : string = failwith "todo" 

    member this.FieldCaption (field:string) : string = failwith "todo" 

    member this.FieldActive (field:string) : bool = failwith "todo" 

    member this.FieldNo (field:string) : int = failwith "todo" 

    member this.SetAscending (field:string,ascending:bool) : unit = failwith "todo" 

    member this.GetAscending (field:string) : bool = failwith "todo" 

    member this.Get (recordId:ALRecordId) : bool = failwith "todo" 

    member this.Mark ()  : bool = failwith "todo" 

    member this.Mark (markValue:bool) : bool = failwith "todo" 

    member this.Init ()  : unit = failwith "todo" 

    member this.Find ()  : unit = failwith "todo" 

    member this.Find (searchMethod:string) : unit = failwith "todo" 

    member this.FindFirst ()  : unit = failwith "todo" 

    member this.FindLast ()  : unit = failwith "todo" 

    member this.FindSet ()  : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool) : unit = failwith "todo" 

    member this.FindSet (forUpdate:bool,updateKey:bool) : unit = failwith "todo" 

    member this.Next ()  : int = failwith "todo" 

    member this.Next (steps:int) : int = failwith "todo" 

    member this.Reset ()  : unit = failwith "todo" 

    member this.LockTable ()  : unit = failwith "todo" 

    member this.LockTable (wait:bool) : unit = failwith "todo" 

    member this.LockTable (wait:bool,versionCheck:bool) : unit = failwith "todo" 

    member this.Consistent (isConsistent:bool) : unit = failwith "todo" 

    member this.GetPosition ()  : string = failwith "todo" 

    member this.GetPosition (useCaptions:bool) : string = failwith "todo" 

    member this.SetPosition (position:string) : unit = failwith "todo" 

    member this.GetView ()  : string = failwith "todo" 

    member this.GetView (useCaptions:bool) : string = failwith "todo" 

    member this.HasLinks : bool = failwith "todo" 

    member this.TableName : string = failwith "todo" 

    member this.TableCaption : string = failwith "todo" 

    member this.Ascending : bool = failwith "todo" 

    member this.Count : int = failwith "todo" 

    member this.CountApprox : int = failwith "todo" 

    member this.IsEmpty : bool = failwith "todo" 

    member this.IsDirty : bool = failwith "todo" 

    member this.CurrentKeyIndex : int = failwith "todo" 

    member this.RecordId : ALRecordId = failwith "todo" 

    member this.GetFilters : string = failwith "todo" 

    member this.CurrentKey : string = failwith "todo" 

    member this.ReadPermission : bool = failwith "todo" 

    member this.WritePermission : bool = failwith "todo" 

    member this.ReadConsistency : bool = failwith "todo" 

    member this.RecordLevelLocking : bool = failwith "todo" 

    member this.FilterGroup : int = failwith "todo" 

    member this.MarkedOnly : bool = failwith "todo" 

    member this.KeyCount : int = failwith "todo" 

    member this.FieldCount : int = failwith "todo" 

    member this.HasFilter : bool = failwith "todo" 

    member this.IsTemporary : bool = failwith "todo" 

    member this.CurrentCompany : string = failwith "todo"
    
    
[<AbstractClass>]
type ALCodeunit() =
    inherit ALObjectValue()

