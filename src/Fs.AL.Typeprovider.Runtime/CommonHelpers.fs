module Fs.AL.Typeprovider.CommonHelpers

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations


[<AutoOpen>]
module ActivePatterns =
    let (|List1|) = function [this] -> this | _ -> failwith "Parameter mismatch"
    let (|List2|) = function [this;value] -> this,value | _ -> failwith "Parameter mismatch"


module TypeDefinition =
    
    let createGenerativeType (asm:Assembly,ns:string) (typeName:string)  =
        let it = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = false)
        it
    let createGenerativeTypeWithBase (asm:Assembly,ns:string) (typeName:string) (baseObjectType:Type) =
        let it = ProvidedTypeDefinition(asm, ns, typeName, Some baseObjectType, isErased = false)
        it
    
    let createMainTypeWithPathParameter (asm:Assembly,ns:string) (typeName:string) (afterPathResolvedFn: string -> string -> ProvidedTypeDefinition)=
        let mainType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = false, hideObjectMethods = true)
        mainType.DefineStaticParameters( 
            [ProvidedStaticParameter("Path", typeof<string>)], 
                fun typeName args -> afterPathResolvedFn typeName (unbox<string> args.[0])
        )
        mainType
        

module Constructor =
    
    let addDefaultConstructor (td:ProvidedTypeDefinition) =
        td.AddMember (ProvidedConstructor(parameters = [], invokeCode = (fun args -> <@@ () :> obj @@>)))
        
    let addDefaultCtorWithBase (baseType:Type) (baseCtorArgTypes:Type list) (baseCtorArgsQuotation:Expr list) (td:ProvidedTypeDefinition) =
        let defaultCtor = ProvidedConstructor(parameters = [], invokeCode = (fun args -> <@@ () :> obj @@>)) //[ProvidedParameter("id", typeof<int>)]
        let ctorInfo = baseType.GetConstructor(baseCtorArgTypes |> List.toArray)
        defaultCtor.BaseConstructorCall <- (fun args -> ctorInfo, args @ baseCtorArgsQuotation )
        td.AddMember defaultCtor
        td

module Property =
    
    let getAutoProperty (propName: string) (propertyType:Type) (targetTypeDefinition: ProvidedTypeDefinition)  =
        let typeProperties = 
            targetTypeDefinition.GetProperties() 
            |> Seq.map (fun prop -> prop.Name)
            |> List.ofSeq

        let propertyName =         
            let checkName name =
                typeProperties |> List.contains name

            if not (checkName propName) then propName
            else 
               let rec getNextProprtyName name count = 
                   let uniqueName = sprintf "%s%i" name count
                   if not (checkName uniqueName) then
                      uniqueName
                   else
                      getNextProprtyName name (count + 1)
               getNextProprtyName propName 1
                
        let providedField = ProvidedField("_" + propertyName, propertyType)
        let providedProperty =
            ProvidedProperty(propertyName, propertyType,
                getterCode = (fun (List1 this) -> Expr.FieldGet(this, providedField)),
                setterCode = (fun (List2 (this,value)) -> Expr.FieldSet(this, providedField, value)))
        providedProperty.AddXmlDoc $"""<summary>Corresponds to property "%s{propName}" in object</summary>"""

        providedField, providedProperty
     
    

        
        

module Attribute =
    
    let addCustomAttributeWithCtor (attrType:Type) (attrCtorParam:'t) (td:ProvidedTypeDefinition) =
        { new CustomAttributeData() with
        member _.Constructor = attrType.GetConstructor ([| typeof<'t> |])
        member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<'t>, attrCtorParam) |]
        member _.NamedArguments = upcast [||] }
        |> td.AddCustomAttribute
        td
        