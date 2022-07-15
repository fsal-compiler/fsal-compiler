[<RequireQualifiedAccess>]
module Fs.AL.Compiler.IntermediateLanguage.ALFunctionReplacements

open FSharp.Compiler.Symbols
open Fs.AL.Compiler.Fullname
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage

        
let replacementFunctions : ( string * ((FSharpExpr -> ALExpression) -> FSharpExpr option -> obj -> FSharpType list -> FSharpExpr list -> ALExpression))[] =
    [|
        // enumerator
        "System.Collections.Generic.List.GetEnumerator",
        (fun toAL obj mem typeArgs2 args -> ALExpression.createMemberAccessInvocation (Identifier "_fs_enumerator") "GetEnumerator" [toAL obj.Value] )
        "System.Collections.Generic.List`1.Enumerator.get_Current",
        (fun toAL obj mem typeArgs2 args -> ALExpression.createMemberAccessInvocation (Identifier "_fs_enumerator") "Current" [toAL obj.Value] )
        "System.Collections.Generic.List`1.Enumerator.MoveNext",
        (fun toAL obj mem typeArgs2 args -> ALExpression.createMemberAccessInvocation (Identifier "_fs_enumerator") "MoveNext" [toAL obj.Value] )
        "System.IDisposable.Dispose",
        (fun toAL obj mem typeArgs2 args -> FSALExpr Ignore)
        // 
        "System.String.get_Length",
        (fun toAL obj mem typeArgs2 args -> ALExpression.createInvocation "StrLen" [toAL obj.Value] )
        "System.Text.Json.JsonSerializer.Deserialize",
        (fun toAL obj mem typeArgs2 args -> ("ReadFrom",[args[0] |> toAL]) |> InvocationWithoutTarget |> FSALExpr )
        "System.Text.Json.Nodes.JsonNode.Parse",
        (fun toAL obj mem typeArgs2 args -> ALExpression.createInvocation "ReadFrom" [toAL obj.Value] )
        "System.DateTime.get_Now",
        (fun toAL obj mem typeArgs2 args -> ALExpression.createInvocation "CurrentDateTime" [] )
        "System.Console.WriteLine",
        (fun toAL obj mem typeArgs2 args -> ALExpression.createMemberAccessInvocation (Identifier "Dialog") "Message" (args |> List.map toAL)  )
        "System.String.Substring",
        (fun toAL obj mem typeArgs2 args ->
            match args[0] with
            // add integer if it's a constant
            | FSharpExprPatterns.Const(v,g) -> ALExpression.createMemberAccessInvocation (toAL obj.Value) "Substring" [Constant (1 + unbox v)]
            | _ -> ALExpression.createMemberAccessInvocation (toAL obj.Value) "Substring" [Binary (Constant 1 ,ALBinaryOperator.Add, toAL args[0])]
        )
    
    |]


let (|HasReplacementFunction|_|)
    (toAL) (objExpOpt) (memberOrFunc:FSharpMemberOrFunctionOrValue) typeArgs2 (argExprs) (s:string) =
    replacementFunctions
    |> Array.tryFind (fun (name,rep) -> name = s)
    |> Option.map (snd)
