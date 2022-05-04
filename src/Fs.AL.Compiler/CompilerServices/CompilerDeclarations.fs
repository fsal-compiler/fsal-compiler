module Fs.AL.Compiler.CompilerDeclarations

open System.Collections.Generic
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Core.Abstract



    

module Helpers =
    let isOverride (mfv:FSharpMemberOrFunctionOrValue) =
        match mfv.ImplementedAbstractSignatures |> Seq.toList with
        | [ x ] -> true
        | [] -> false
        | _ -> failwithf $"multiple overrides not implemented"
        
        
    let objectvaluefulltype = typeof<ALObjectValue>.FullName.Replace("+",".")  
    let isALObjectPropertyOverride (mfv:FSharpMemberOrFunctionOrValue) =
        match mfv.ImplementedAbstractSignatures |> Seq.toList with
        | [ x ] -> x.DeclaringType.TypeDefinition.FullName = objectvaluefulltype
        | [] -> false
        | _ -> failwithf $"multiple overrides not implemented"
    

    
    

module FSharpImplementation =
    
    type private t = FSharpImplementationFileDeclaration
    
    
    let private getTypeMembers (x:t) =
        let rec loop acc (xs:t list) =
            match xs with
            | [] -> acc |> List.rev
            | head :: tail ->
                match head with 
                | t.Entity(entity, implementationFileDeclarations) ->
                    let tail' = List.append implementationFileDeclarations tail
                    loop acc tail'
                | t.MemberOrFunctionOrValue(mfv, curriedArgs, body) ->
                    let enclosingEntity = mfv.ApparentEnclosingEntity
                    match enclosingEntity.TryGetAttribute<ALSingleInstanceCodeunit>() with
                    | Some si ->
                        let entityMember = (mfv, curriedArgs, body)
                        loop ((enclosingEntity,entityMember)::acc) tail     
                    | None ->
                        let isALType = FSharpType.isInheritedFromALType (enclosingEntity.BaseType)
                        if not isALType then loop acc tail else
                        match mfv with
                        | x when x.IsConstructor 
                          || not x.IsMember -> loop acc tail
                        | x when x.IsValue ->
                            loop acc tail
                        | x when x.IsFunction ->
                            let entityMember = (mfv, curriedArgs, body)
                            loop ((enclosingEntity,entityMember)::acc) tail 
                        | _ -> failwithf $"%A{mfv}"                    
                | t.InitAction action ->
                    printfn $"action:%A{action}"
                    loop acc tail
            
        loop [] [x]
        
    let ofImplementationFileDeclaration (x: t) =
        x 
        |> getTypeMembers
        |> Seq.groupBy fst
        |> Seq.map (fun (entity,entitymembers) ->
            entity,
            entitymembers |> Seq.map snd |> Seq.toArray
        )
        
            
        