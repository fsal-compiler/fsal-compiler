module Fs.AL.Compiler.CompilerDeclarations

open System
open System.Collections.Generic
open System.Numerics
open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Core.ALCoreValues
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
    
    let private shouldCollectMfv (mfv:FSharpMemberOrFunctionOrValue) =
        match mfv with
        | x when x.IsCompilerGenerated ->
            match x.IsOverrideOrExplicitInterfaceImplementation with
            | true ->
                match x.CompiledName with
                // skip F# record overrides
                | "CompareTo" | "GetHashCode" | "Equals" -> false
                | _ -> true
            | false -> raise (NotImplementedException())
        //        | x when x.CompiledName = "get_ObjectId" -> false
        | x when x.IsConstructor  -> false
        //&& not x.IsMember
        | x when x.IsFunction -> true
        | x when x.IsValue -> false
        | _ -> failwith "unknown val"  
             
    type FSharpEntityImpl =
        {
            entity : FSharpEntity
            members : (FSharpMemberOrFunctionOrValue * FSharpMemberOrFunctionOrValue list list * FSharpExpr) ResizeArray
        }
    let private getEntityImpls (x:t) =
        let rec loop acc (xs:t list) =
            match xs with
            | [] -> acc //|> List.map (fun f -> {f with members = f.members |> List.rev }) 
            | head :: tail ->
                match head with 
                | t.Entity(entity, implementationFileDeclarations) ->
                    let tail' = List.append implementationFileDeclarations tail
                    let addEntity() = loop ({entity=entity;members=ResizeArray()}::acc) tail'
                    
                    if entity.IsFSharpAbbreviation then loop acc tail' else
                    
                    match entity |> FSharpEntity.tryGetALObjectKind with
                    | Some (ALType.Complex (Record name)) -> addEntity()  
                    | Some (ALType.Complex (Codeunit name)) -> addEntity()
                    | Some n -> raise (NotImplementedException())
                    | _ -> loop acc tail'
                | t.MemberOrFunctionOrValue(mfv, curriedArgs, body) ->
                    let enclosingEntity = mfv.ApparentEnclosingEntity
                    if enclosingEntity.CompiledName = "AzureFnTest" then
                        let d = 1
                        ()
                    let collectMembers() =
                        if shouldCollectMfv mfv then
                            let entityMember = (mfv, curriedArgs, body)
                            match acc |> List.tryFind (fun f -> f.entity = enclosingEntity) with
                            | Some impl -> impl.members.Add(entityMember)  
                            | None -> raise (NotImplementedException())
                            loop acc tail 
                        else loop acc tail 
                    
                    match enclosingEntity |> FSharpEntity.tryGetALObjectKind with
                    | Some (ALType.Complex (Record name)) -> collectMembers()   
                    | Some (ALType.Complex (Codeunit name)) -> collectMembers()
                    | Some othertype ->
                        let v = 1
                        raise (NotImplementedException())
                    | None ->
                        let isALType = FSharpType.isInheritedFromALType (enclosingEntity.BaseType)
                        if not isALType then loop acc tail else
                        collectMembers()
                | t.InitAction action ->
                    printfn $"action:%A{action}"
                    loop acc tail
            
        loop [] [x]
        
    let ofImplementationFileDeclaration (x: t) =
        let typesWithMembers = x |> getEntityImpls
        typesWithMembers
//            |> Seq.groupBy fst
//            |> Seq.map (fun (entity,entitymembers) ->
//                entity, entitymembers |> Seq.map snd |> Seq.toArray
//            )
//        let records = 
//            x |> List.where (fun f -> f)
//            
        