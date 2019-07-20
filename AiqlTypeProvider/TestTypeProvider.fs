namespace TestProvider

open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.Extensions

[<TypeProvider>]
type TestTypePrivider (config : TypeProviderConfig) as this =   
    inherit TypeProviderForNamespaces (config, addDefaultProbingLocation=true)
    let ns = "TestProvider"
    let asm = ProvidedAssembly()

    let mainType = ProvidedTypeDefinition(asm, ns, "TestMainType",  Some typeof<obj>, isErased=false)

    let createTypes typeName z =
        let recordType = ProvidedRecordDefnition(asm, ns, "TestType", ["prop1", typeof<string>; "prop2", typeof<int>])
        asm.AddTypes ([mainType;recordType])
        recordType :> ProvidedTypeDefinition

    let staticParams = [ProvidedStaticParameter("blah", typeof<string>)]

    do mainType.DefineStaticParameters(
        parameters=staticParams,
        instantiationFunction= createTypes)
    
    do this.AddNamespace(ns, [mainType])

