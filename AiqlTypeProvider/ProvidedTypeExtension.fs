namespace ProviderImplementation.ProvidedTypes

open System
open System.Reflection

//type ProvidedRecordProperty(propertyName, propertyType, ?getterCode, ?setterCode, ?isStatic, ?indexParameters) =
    //inherit PropertyInfo()
//    inherit ProvidedProperty(propertyName, propertyType, ?getterCode = getterCode, ?setterCode = setterCode, ?isStatic = isStatic, ?indexParameters = indexParameters)
type ProvidedRecord(className:string, baseType, ?hideObjectMethods, ?nonNullable, ?isErased) =
    inherit ProvidedTypeDefinition(className, baseType, ?hideObjectMethods = hideObjectMethods, ?nonNullable = nonNullable, ?isErased = isErased)
    
    let recordAttribs = [|(CompilationMappingAttribute(SourceConstructFlags.RecordType) :> Attribute)|] |> box |> unbox<obj[]>
        
    override this.GetCustomAttributes(_inherit) = recordAttribs
    override this.GetCustomAttributes(_attributeType, _inherit) = recordAttribs
    
    member this.RecordFields =
        this.GetProperties()
        //|> Array.choose (function :? ProvidedRecordProperty as prp -> Some (prp :> PropertyInfo) | _ -> None)
        |> Array.choose (function :? ProvidedProperty as prp -> Some (prp :> PropertyInfo) | _ -> None)
        |> Array.toList