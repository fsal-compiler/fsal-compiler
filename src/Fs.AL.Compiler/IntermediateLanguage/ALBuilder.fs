module rec Fs.AL.Compiler.ALBuilder

open FSharp.Compiler.Symbols
open Fs.AL.Compiler.CompilerDeclarations
open Fs.AL.Compiler.IntermediateLanguage.ALContext
open Fs.AL.Compiler.IntermediateLanguage.ALLanguage
open Fs.AL.Compiler.CompilerSymbols
open Fs.AL.Compiler.IntermediateLanguage
open Fs.AL.Compiler.Reflection
open Fs.AL.Core.ALCoreValues
open Fs.AL.Core.Abstract
open Microsoft.Dynamics.Nav.CodeAnalysis.Syntax
    
 

let toALVariable (mfv:FSharpMemberOrFunctionOrValue) =
    let ismutable = mfv.FullType.TypeDefinition |> FSharpType.isRefType
    let typedefinition = mfv.FullType |> ALType.ofFSharpType
    let argname = mfv.DisplayName
    {isMutable = ismutable; name =argname; altype = typedefinition}

type ALMemberBuilder =
    | Procedure of ALProcedureContext
    
type ALObjectBuilder with
    static member withALMember (b:ALObjectBuilder) (fsmember:(FSharpMemberOrFunctionOrValue * FSharpMemberOrFunctionOrValue list list * FSharpExpr)) =
        let (mem,args,body) = fsmember
        
        let returnvalue = 
            match mem.ReturnParameter.Type with
            //| x when x |> FSharpType.hasBaseType<IJsonDocument> -> Some (Simple (SimpleType "JsonToken"))
            | x when x.IsFunctionType -> Some (mem.ReturnParameter.Type |> ALType.ofFSharpType)
            | x when x.TypeDefinition.Assembly.QualifiedName = "" ->
                if mem.Assembly.SimpleName <> "" then 
                    Some (mem.ReturnParameter.Type |> ALType.ofFSharpType)
                else failwithf $"unimplemented"
            | x when x |> FSharpType.isUnit -> None 
            | _ -> Some (mem.ReturnParameter.Type |> ALType.ofFSharpType)   
        
        let parameters =
            let paramsFlattened =
                let a = 5
                args |> Seq.collect id
                |> Seq.where (fun f ->
                    f.FullType |> FSharpType.getFullName <> FullNameFSharp.unit)
            match mem.IsInstanceMember with
            | true -> paramsFlattened |> Seq.skip 1 
            | false -> paramsFlattened
            |> Seq.map toALVariable
            |> Seq.toList
        
        
            
        let procedureBuilder =
            { ALProcedureContext.Default with
                isLocal= false // TODO: implement
                identifier = mem.DisplayName
                parameters = parameters
                entity = mem.ApparentEnclosingEntity
                returnType = returnvalue
            }
      
        // for debugging      
//        let onstatementAdd =
//            procedureBuilder.statements.CollectionChanged
//            |> Observable.filter (fun f -> f <> null )
//            |> Observable.add (fun f -> ())
                           
        let newcontext = ExpressionReader.readProcedureBody procedureBuilder body
        
        
        
        // only needed for DateTime.Now for now
        match returnvalue with
        | None -> () 
        | Some alType -> 
            let laststatement = newcontext.statements |> Seq.last
            match laststatement with
            | Expression (Constant o) -> // change last statement to exit condition
                newcontext.statements.Remove(laststatement) |> ignore
                newcontext.statements.Add(Exit(Constant o))
            | Expression (NaryExpression (Invocation(alExpression, alExpressions))) ->
                newcontext.statements.Remove(laststatement) |> ignore
                newcontext.statements.Add(Exit(NaryExpression (Invocation(alExpression, alExpressions))))
            | Expression (Binary(alExpression, alBinaryOperator, expression)) ->
                newcontext.statements.Remove(laststatement) |> ignore
                newcontext.statements.Add(Exit(Binary(alExpression, alBinaryOperator, expression)))
            | Expression (Identifier s) ->
                newcontext.statements.Remove(laststatement) |> ignore
                newcontext.statements.Add(Exit((Identifier s)))
            //todo: if statement
            | Exit (stat) -> ()
            | _ -> failwithf $"unimplemented case: %A{laststatement}"
        { b with alMembers = Procedure newcontext::b.alMembers }
        
        
        
        
type ALFieldBuilder =
    | RecordField of ALRecordFieldContext

type ALObjectBuilder with
    static member withALField (context: ALObjectBuilder) (fsmember:(FSharpMemberOrFunctionOrValue )) =
        let (mem) = fsmember
        
        match mem.IsProperty with 
        | false -> failwithf $"bug"
        | true ->
            let oldmemtype = mem.FullType |> ALType.ofFSharpType
            let memname = mem.DisplayName
            
            let currentFieldId = context.nextFieldId
                
            
            let len =
                mem.Attributes
                |> Seq.tryFind (fun f -> f.AttributeType.CompiledName = "MaxLengthAttribute")
                |> Option.map (fun f -> f.ConstructorArguments[0] |> snd :?> int)
                      //|> Seq.tryFind (fun f -> f.AttributeType")
            
            let memtype =
                match len,oldmemtype with
                | None,_ -> oldmemtype
                | Some l, Simple (Text None) -> Simple (Text (Some l))
                | Some l, Simple (Code None) -> Simple (Code (Some l))
                | _ -> failwithf $"%A{oldmemtype}"
            
            let alField = 
                {
                  ALRecordFieldContext.Default with
                    id = currentFieldId
                    altype = memtype
                    identifier = memname
                    properties = []
                    length = len
                }
                |> RecordField
                
            { context with
                nextFieldId = context.nextFieldId + 1
                alFields = alField::context.alFields
            } 
            
            
            
            
            

        
            
type SharedBuilderContext =
    {
        abstractClasses : ALObjectBuilder[]
        outputPath : string
    }        

type ALObjectBuilder = {
        fsharpEntity : FSharpEntity 
        fsharpMembers :  (FSharpMemberOrFunctionOrValue * FSharpMemberOrFunctionOrValue list list * FSharpExpr)[]
        objectId : int
        sharedCache : SharedBuilderContext 
        alMembers : ALMemberBuilder list
        alFields : ALFieldBuilder list
        mutable nextFieldId : int
        
}
with
    static member ofEntityWithMembers entityWithMembers = {
        fsharpEntity = fst entityWithMembers
        fsharpMembers = snd entityWithMembers
        objectId = 0
        sharedCache = {
            abstractClasses = [||]
            outputPath = ""
        }
        alMembers = [] 
        alFields = []
        nextFieldId = 1
    }
       
    static member withCache (cache:SharedBuilderContext) (b:ALObjectBuilder) =
        { b with sharedCache = cache }
        
    /// adds object id from overridden property
    static member withDefaultObjectId (b:ALObjectBuilder) =
        // TODO: add more options to configure object ID
        let fail() = failwithf $"Couldn't resolve AL Object ID for %A{b.fsharpEntity}"
        let reference = Unchecked.defaultof<ALObjectValue>
        b.fsharpMembers
        |> Seq.tryFind (fun (mem,_,_) -> mem.DisplayName = nameof reference.ObjectId )
        |> function 
        | Some (mem,args,body) ->
            match body with
            | FSharpExprPatterns.Const (objval,objType) ->
                match objval with
                | :? int as v -> v // declared as override Id member
                | _ -> fail()
            | _ -> fail()
        | _ ->
            if b.fsharpEntity.IsAbstractClass then 0 else
            match b.fsharpEntity.TryGetAttribute<ALSingleInstanceCodeunit>() with
            | None -> failwithf $"%A{b}"
            | Some attr -> attr.ConstructorArguments[0] |> snd :?> int
        |> (fun f ->
            {b with objectId = f}
        ) 
    
    /// add procedures and triggers    
    static member withALMembers (b:ALObjectBuilder) =
        let fsmembers = b.fsharpMembers
        let objectValueFullType = typeof<ALObjectValue>.FullName.Replace("+",".") //runtime type
        fsmembers
        |> Seq.where (fun (mem,_,_) ->
            not mem.IsPropertyGetterMethod
            && not mem.IsPropertySetterMethod
        )
        |> Seq.where (fun (mem,_,_) ->
            match mem.ImplementedAbstractSignatures |> Seq.toList with
            | [] -> true
            | [x] -> // inherited from abstract type e.g. override ObjectId
                match x.DeclaringType.TypeDefinition.FullName with
                | x when x = objectValueFullType -> false
                | _ -> true
            | _ -> failwith "unimplemented case"
        )
        |> Seq.fold ALObjectBuilder.withALMember b
        |> (fun builder ->
            { builder with alMembers = List.rev builder.alMembers}
        )
               
        
        
    /// add inherited members
    static member withInheritedMembers (b:ALObjectBuilder) =
        match b.fsharpEntity with
        | t when t.IsFSharpModule -> b
        | ent ->
            match ent.BaseType with 
            | None -> failwith "shouldnt be possible"
            | t when t |> FSharpType.isDirectlyInheritedFromALType -> b // is base AL type
            // has inherited type
            | Some t ->
                let baseEntity = t.TypeDefinition
                let baseEntityMembers =
                    b.sharedCache.abstractClasses
                    |> Seq.find (fun f -> f.fsharpEntity = baseEntity)
                    |> (fun f -> f.fsharpMembers)
                { b with fsharpMembers = b.fsharpMembers |> Array.append baseEntityMembers} 
            
    /// add table fields
    static member withALFields (b:ALObjectBuilder) =
        let inheritedFields : _ seq =
            match b.fsharpEntity.BaseType with
            | None -> failwith "shouldnt be possible"
            | t when t |> FSharpType.isDirectlyInheritedFromALType -> [||] // is base AL type
            | Some t -> // has inherited type
                let baseEntity = t.TypeDefinition
                baseEntity.MembersFunctionsAndValues
                |> Seq.where (fun f -> f.IsPropertyGetterMethod)
                |> Seq.map (fun f ->
                    baseEntity.MembersFunctionsAndValues
                    |> Seq.where (fun d -> d.HasGetterMethod )
                    |> Seq.find (fun d -> d.GetterMethod = f)
                )
                
        let fields =
            b.fsharpEntity.MembersFunctionsAndValues
            |> Seq.where (fun f -> f.IsPropertyGetterMethod)
            |> Seq.map (fun f ->
                b.fsharpEntity.MembersFunctionsAndValues
                |> Seq.where (fun d -> d.HasGetterMethod ) 
                |> Seq.find (fun d -> d.GetterMethod = f) )
            
        let updatedBuilder =
            fields
            |> Seq.append inheritedFields  
            |> Seq.where (Helpers.isALObjectPropertyOverride >> not)
            |> Seq.fold ALObjectBuilder.withALField b
            
            
        let tt = 5
//        let folded = updatedBuilder |> List.fold ALObjectBuilder.withALField b
            
        { updatedBuilder with alFields = updatedBuilder.alFields |> List.rev}
    
    
type CompilerContext =
    {
        builders : ALObjectBuilder array
        cache : SharedBuilderContext
    }    