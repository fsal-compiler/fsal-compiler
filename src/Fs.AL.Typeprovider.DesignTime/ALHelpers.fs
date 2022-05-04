module Fs.AL.Typeprovider.DesignTime.ALHelpers


//let (>>=) x f =
//    Option.bind f x


open System.Reflection
open Fs.AL
open Fs.AL.Core.ALComplexValues
open Fs.AL.Typeprovider
open Microsoft.Dynamics.Nav.CodeAnalysis.SymbolReference
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations



module TableDefinition =
    type RecordKey = {
        fields:(string*string)[]
        keyname:string
        desc:string
    }
    let getKeys (table:TableDefinition) =
        table.Keys 
        |> Option.ofObj 
        |> Option.defaultValue [||]
        |> Array.map (fun key ->
            let keyFields = 
                key.FieldNames
                |> Array.choose (fun g ->
                    table.Fields 
                    |> Array.tryFind (fun d -> d.Name = g)
                    |> Option.map (fun f -> 
                        let typedef = f.TypeDefinition.Name
                        f.Name,typedef
                    )
                )
                |> Array.distinctBy (fun (nam,def) -> def )
            let keyDescription =
                key.Properties
                |> Option.ofObj
                |> Option.map (fun f -> 
                    f |> Array.tryFind (fun d -> d.Name = "Description")
                )
                |> Option.flatten
                |> Option.map (fun f -> f.Value)
                |> Option.defaultValue ""
            {
                fields = keyFields
                keyname = key.Name
                desc = keyDescription
            }
        )
        
    let tryGetPrimaryKey (table:TableDefinition) =
        
        table.Keys 
        |> Option.ofObj 
        |> Option.bind Seq.tryHead
        |> Option.map (fun key ->
            let keyFields = 
                key.FieldNames
                |> Array.choose (fun g ->
                    table.Fields 
                    |> Array.tryFind (fun d -> d.Name = g)
                    |> Option.map (fun f -> 
                        let typedef = f.TypeDefinition.Name
                        f.Name,typedef
                    )
                )
            let keyDescription =
                key.Properties
                |> Option.ofObj
                |> Option.map (fun f -> 
                    f |> Array.tryFind (fun d -> d.Name = "Description")
                )
                |> Option.flatten
                |> Option.map (fun f -> f.Value)
                |> Option.defaultValue ""
            {
                fields = keyFields
                keyname = key.Name
                desc = keyDescription
            }
        )
    
    
    
        
    let createALTable (asmns) (tbl:TableDefinition) =
        try 
            let tableId = tbl.Id.Value
            let typeDefinition =
                CommonHelpers.TypeDefinition.createGenerativeTypeWithBase (asmns) tbl.Name typeof<ALRecord>
                
            // add ctor, base ctor immediately
            typeDefinition
            |> CommonHelpers.Constructor.addDefaultCtorWithBase typeof<ALRecord> [  ] [  ]
            |> ignore
            
            // override ObjectId 
            let overrideObjectId =
                ProvidedMethod("get_ObjectId", [], typeof<int>, isStatic=false, 
                    invokeCode = (fun args -> Expr. Value(tableId, typeof<int>)))
            overrideObjectId.SetMethodAttrs(MethodAttributes.Virtual|||MethodAttributes.MemberAccessMask)

            let getProcedure =
                tbl
                |> tryGetPrimaryKey
                |> Option.map (fun f ->
                    let parameters =
                        f.fields
                        |> Array.toList
                        |> List.map (fun (keyFieldName,keyFieldType) ->
                            let fieldType = BcTypes.fromBcTypeDefinitionName keyFieldType
                            ProvidedParameter (keyFieldName, fieldType)
                        )

                    ProvidedMethod("Get", parameters, typeof<bool>, isStatic=false,
                        invokeCode = (fun args -> Expr.Value(tableId, typeof<int>))
                    )
                )
            
            let getMembers =
                [
                    yield overrideObjectId
                    
                    if getProcedure.IsSome then
                        yield getProcedure.Value
                    
                    for field in tbl.Fields do
                        let field,prop = typeDefinition |> CommonHelpers.Property.getAutoProperty field.Name (BcTypes.fromBcTypeDefinitionName field.TypeDefinition.Name)
                        yield field 
                        yield prop 
                ]
                : MemberInfo list
            // add table fields
            typeDefinition.AddMembers(getMembers)
            typeDefinition
        with e -> 
            Logger.log (sprintf "%s %s" e.Message e.StackTrace)
            failwith "ff"