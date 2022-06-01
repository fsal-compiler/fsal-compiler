module rec Fs.AL.Compiler.IntermediateLanguage.ALContext

open System.Collections.ObjectModel
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.Reflection
open Fs.AL.Compiler.Visitors
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract
open Microsoft.Dynamics.Nav.CodeAnalysis.Syntax
    


// Tom Hvitved - Architectural Analysis of Microsoft Dynamics NAV
// Page 30 - definition of procedure

[<RequireQualifiedAccess>]
type ALExprContext =
    | NoContext
    | Sequential of chain:FSharpExpr list
    | BindingVar of FSharpMemberOrFunctionOrValue
    | LetBinding of var:FSharpMemberOrFunctionOrValue * bindingExpr:FSharpExpr
    | DecisionTree of decisionExpr:FSharpExpr * targets:ALExpression list
    | Constructor of identifier:string
    static member usingCtx (ctx:ALExprContext) (b:ALProcedureContext) fn  =
        b.expressionContext.Add(ctx)
        let result = fn b
        b.expressionContext.Remove(ctx) |> ignore
        result
        
    static member getCtorCtx (b:ALProcedureContext) =
        b.expressionContext
        |> Seq.choose (fun f ->
            match f with
            | ALExprContext.Constructor identifier -> identifier |> Some     
            | _ -> None
        )
        |> Seq.tryLast
        

type ALProcedureContext =
    {
        isLocal : bool
        identifier : string
        parameters : ALVariable list
        localVariables: ObservableCollection<ALVariable>
        mutable statements : ALStatement
        returnType : ALType option
        entity : FSharpEntity
        mutable expressionContext : ObservableCollection<ALExprContext>
    }
    static member Default = {
        isLocal = false
        identifier = ""
        parameters = []
        localVariables = ObservableCollection()
        statements = FSALExpr Ignore |> Expression
        returnType = None
        entity = Unchecked.defaultof<FSharpEntity>
        expressionContext = ObservableCollection()
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
    
    
module ALProcedureContext =
    
    let private addIfNotExists b item ifNot =
        let alVariableOption = 
            b.localVariables
            |> Seq.tryFind (fun f -> f.name = item)
        match alVariableOption with  
        | Some _ -> ()
        | None -> b.localVariables.Add(ifNot)
        
        
    let ensureHasVariable (b:ALProcedureContext) var =
        addIfNotExists b var.name var
            
            
    let addBindingVar (b:ALProcedureContext) (variable:FSharpMemberOrFunctionOrValue)=
        {
            ALVariable.isMutable = false
            name = variable.DisplayName
            altype = variable.FullType |> ALType.ofFSharpType
        }
        |> b.localVariables.Add