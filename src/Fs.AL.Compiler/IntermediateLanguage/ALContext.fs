module Fs.AL.Compiler.IntermediateLanguage.ALContext

open System.Collections.ObjectModel
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.Reflection
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract
open Microsoft.Dynamics.Nav.CodeAnalysis.Syntax
    


// Tom Hvitved - Architectural Analysis of Microsoft Dynamics NAV
// Page 30 - definition of procedure

type ALProcedureContext =
    {
        isLocal : bool
        identifier : string
        parameters : ALVariable list
        localVariables: ObservableCollection<ALVariable>
        statements : ObservableCollection<ALStatement>
        returnType : ALType option
        entity : FSharpEntity
    }
    static member Default = {
        isLocal = false
        identifier = ""
        parameters = []
        localVariables = ObservableCollection()
        statements = ObservableCollection()
        returnType = None
        entity = Unchecked.defaultof<FSharpEntity>
    }
    
    
type ALRecordFieldContext =
    {
        id : int
        identifier : string
        altype : ALType
        length : int option
        properties : (string*obj) list
    }
    static member Default = {
        id = 0
        identifier = ""
        altype = Simple Integer
        length = None
        properties = []
    }