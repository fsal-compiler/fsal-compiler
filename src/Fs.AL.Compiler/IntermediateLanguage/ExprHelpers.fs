[<Microsoft.FSharp.Core.AutoOpen>]
module rec Fs.AL.Compiler.IntermediateLanguage.ExprHelpers

open System.Collections.Generic
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.Fullname
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Core.ALCoreValues

module JsonTokens =
    let private getFirstCast fullname =
        match fullname with
        | Types.FsArray -> "AsArray"
        | Types.String 
        | Types.Int32 
        | Types.Boolean 
        | Types.Double -> "AsValue"
        | Types.JsonValue -> "AsObject"
        | Types.JsonArray -> "AsArray"
        | x when x.StartsWith "Fable.JsonProvider.Generator<...>" -> "AsObject"
        | x -> failwithf $"unimplemented case %A{x}"
    
    let private getSecondCast fullname =
        match fullname with
        | Types.FsArray -> "___"
        | Types.JsonArray -> "___"
        | Types.String -> "AsText"
        | Types.Int32 -> "AsInteger"
        | Types.Boolean -> "AsBoolean"
        | Types.Double -> "AsDecimal"
        | Types.JsonValue -> "AsToken"
        | x when x.StartsWith "Fable.JsonProvider.Generator<...>" -> "AsToken"
        | x -> failwithf $"unimplemented case %A{x}"
        
    let getJsonCastExpr (identifier:ALExpression) (fullType:FSharpType) = //letBindVarJsonName
        let fullTypeName = fullType |> FSharpType.getRootType |>  FSharpType.getFullName
        let _castMethod1 = fullTypeName |> getFirstCast
        let _castMethod2 = fullTypeName |> getSecondCast
        let firstCast = ALExpression.createMemberAccessInvocation identifier _castMethod1 []
        match fullTypeName with
        |  Types.FsArray ->
            
            firstCast
        |  Types.JsonArray -> firstCast
        |  x when x.StartsWith "Fable.JsonProvider.Generator<...>" -> identifier
        | _ -> 
            ALExpression.createMemberAccessInvocation firstCast _castMethod2 []
 



module ALStatement =
    let createSelectToken (identifier:string) (args) =
        ALExpression.createMemberAccessInvocation
            (Identifier identifier)
            "SelectToken"
            args
        |> ALStatement.ofExpression
      
    let createValAssignment (mfv:FSharpMemberOrFunctionOrValue) (assigmentExpr:ALExpression) =
        Assignment(
            Identifier mfv.DisplayName,
            assigmentExpr
        )
    let createAssignment (identifier:string) (assigmentExpr:ALExpression) =
        Assignment(
            Identifier identifier,
            assigmentExpr
        )
        
    let createMemberCall (identifier:string) memberName args =
        NaryExpression (Invocation( Binary (
            Identifier identifier,
            ALBinaryOperator.MemberAccess,
            Identifier memberName)
                ,args)
        )
        |> ALStatement.ofExpression
    
    
[<RequireQualifiedAccess>]    
module ALExpression =
    
    module Value =
        let isReference (exp:FSharpExpr) =
            match exp with
            | FSharpExprPatterns.Value (valueToGet) ->
                valueToGet.FullType.AbbreviatedType.TypeDefinition.FullName
                    = "Microsoft.FSharp.Core.FSharpRef`1"
            | _ -> false
            
    
    let isNewObject (exp:FSharpExpr) =
        match exp with
        | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) ->  true
        | _ -> false
        
    let isLetExpr (exp:FSharpExpr) =
        match exp with
        | FSharpExprPatterns.Let(bindingExpr, nextExpr) ->  true
        | _ -> false
        
    let isThis (exp:FSharpExpr) =
        match exp with
        | FSharpExprPatterns.Value(valueToGet) ->
            valueToGet.DisplayName = "this"
        | _ -> false
        
    let convertTo1BasedIndex (exp:ALExpression) =
        match exp with
        | Constant o -> Constant (1 + (o :?> int))
        | Identifier s -> ALExpression.createBinary (Identifier s) Add (Constant 1)
        | _ -> failwith "can not convert to 1 based index"          
    let getConstValue (exp:ALExpression) =
        match exp with
        | Constant o -> o 
        | _ -> failwith "not a constant expresison"    
    
    let createJsonCastExpr (identifier:string) (fsType:FSharpType) =
        JsonTokens.getJsonCastExpr (Identifier identifier) (fsType)
            
    let createIdentifer (mfv:FSharpMemberOrFunctionOrValue)  =
        Identifier mfv.DisplayName
    
    let createBinary (targetExpr:ALExpression) operator (func:ALExpression)  =
        Binary (targetExpr,operator,func)
    
    let createMemberAccess targetExpr membername  =
        Binary (targetExpr,ALBinaryOperator.MemberAccess,Identifier membername)
    
    /// e.g. (Identifier <targetName>).<memberName>(args)
    let createMemberAccessInvocation targetExpr memberName args =
        NaryExpression (Invocation( Binary (
            targetExpr,ALBinaryOperator.MemberAccess,Identifier memberName) ,args))
        
    /// e.g. <methodName>(args)
    let createInvocation methodName args =
        NaryExpression (Invocation(
            (Identifier methodName),args))
        
        
    module Invocation =
        let addArgument (arg:ALExpression) (invocExpr:ALExpression) =
            match invocExpr with
            | NaryExpression alNaryExpression ->
                match alNaryExpression with
                | Invocation(alExpression, alExpressions) ->
                    Invocation(alExpression,alExpressions @ [arg])
                    |> NaryExpression
                | _ -> failwith "unimplemented case"
            | _ -> failwith "unimplemented case"
                
        let changeArgument (arg:ALExpression) (invocExpr:ALExpression) =
            match invocExpr with
            | NaryExpression alNaryExpression ->
                match alNaryExpression with
                | Invocation(alExpression, alExpressions) ->
                    Invocation(alExpression,[arg])
                    |> NaryExpression
                | _ -> failwith "unimplemented case"
            | _ -> failwith "unimplemented case"
                
                
module ALVariable =
    
    let createSimple name (simpleType:ALSimpleType) =
        {
            name = name
            isMutable = false
            altype = Simple simpleType
        }
        
    let createComplex name (complexType:ALComplexType) =
        {
            name = name
            isMutable = false
            altype = Complex complexType
        }
        
    
    /// compiler generated json token
    let createGenJsonToken target targetJsonProp (localvars:ICollection<ALVariable>) =
        let intermediaryVarName = $"@j@{target}.{targetJsonProp}_{localvars.Count}"
        {   name = intermediaryVarName
            isMutable = false
            altype = Simple JsonToken }
        
    let createGenField target targetProp alType (localvars:ICollection<ALVariable>) =
        let targetVarName = $"@{target}.{targetProp}_{localvars.Count}"
        {   name = targetVarName
            isMutable = false
            altype = alType }
