[<RequireQualifiedAccess>]
module Fs.AL.Compiler.IntermediateLanguage.ALFunctionReplacements

open FSharp.Compiler.Symbols
open Fs.AL.Compiler.Fullname
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage

        
let replacementFunctions : ( string * ((FSharpExpr -> ALExpression) -> FSharpExpr option -> obj -> FSharpExpr list -> ALExpression))[] =
    [|
        // enumerator
        "System.Collections.Generic.List.GetEnumerator",
        (fun toAL obj mem args -> ALExpression.createMemberAccessInvocation (Identifier "_fs_enumerator") "GetEnumerator" [toAL obj.Value] )
        "System.Collections.Generic.List`1.Enumerator.get_Current",
        (fun toAL obj mem args -> ALExpression.createMemberAccessInvocation (Identifier "_fs_enumerator") "Current" [toAL obj.Value] )
        "System.Collections.Generic.List`1.Enumerator.MoveNext",
        (fun toAL obj mem args -> ALExpression.createMemberAccessInvocation (Identifier "_fs_enumerator") "MoveNext" [toAL obj.Value] )
        "System.IDisposable.Dispose",
        (fun toAL obj mem args -> FSALExpr Ignore)
        // 
        "System.String.get_Length",
        (fun toAL obj mem args -> ALExpression.createInvocation "StrLen" [toAL obj.Value] )
        "System.Text.Json.JsonSerializer.Deserialize",
        (fun toAL obj mem args -> ALExpression.createInvocation "ReadFrom" [toAL obj.Value] )
        "System.Text.Json.Nodes.JsonNode.Parse",
        (fun toAL obj mem args -> ALExpression.createInvocation "ReadFrom" [toAL obj.Value] )
        "System.DateTime.get_Now",
        (fun toAL obj mem args -> ALExpression.createInvocation "CurrentDateTime" [] )
        "Fs.AL.Core.ALFunctions.ALDialog.Message",
        (fun toAL obj mem args -> ALExpression.createMemberAccessInvocation (Identifier "Dialog") "Message" (args |> List.map toAL)  )
        "Fs.AL.Core.ALFunctions.ALDialog.Error",
        (fun toAL obj mem args -> ALExpression.createMemberAccessInvocation (Identifier "Dialog") "Error" (args |> List.map toAL)  )
        "System.Console.WriteLine",
        (fun toAL obj mem args -> ALExpression.createMemberAccessInvocation (Identifier "Dialog") "Message" (args |> List.map toAL)  )
        "System.String.Substring",
        (fun toAL obj mem args ->
            match args[0] with
            // add integer if it's a constant
            | FSharpExprPatterns.Const(v,g) -> ALExpression.createMemberAccessInvocation (toAL obj.Value) "Substring" [Constant (1 + unbox v)]
            | _ -> ALExpression.createMemberAccessInvocation (toAL obj.Value) "Substring" [Binary (Constant 1 ,ALBinaryOperator.Add, toAL args[0])]
        )
    
    |]


let (|HasReplacementFunction|_|)
    (toAL) (objExpOpt) (memberOrFunc:FSharpMemberOrFunctionOrValue) (argExprs) (s:string) =
    replacementFunctions
    |> Array.tryFind (fun (name,rep) -> name = s)
    |> Option.map (snd)
