module AiqlTypeProviderTests

open ProviderImplementation.ProvidedTypesTesting

open System
open System.IO



let (++) a b = Path.Combine(a, b)

let sourceDirectory = __SOURCE_DIRECTORY__


let expectedDirectory = sourceDirectory ++ "expected"

let resolutionFolder = sourceDirectory ++ ".." ++ "FSharp.Data.Tests" ++ "Data"
let assemblyName = "FSharp.Data.dll"
let net45RuntimeAssembly = sourceDirectory ++ ".." ++ ".." ++ "bin" ++ "lib" ++ "net45" ++ assemblyName
let netstandard2RuntimeAssembly = sourceDirectory ++ ".." ++ ".." ++ "bin" ++ "lib" ++ "netstandard2.0" ++ assemblyName



let getRuntimeRefs = Targets.DotNetStandard20FSharp45Refs()

()
//let typeIntance = Testing.GenerateProvidedTypeInstantiation(resolutionFolder , getRuntimeRefs,  )

//let generateAllExpected() =
//    if not <| Directory.Exists expectedDirectory then
//        Directory.CreateDirectory expectedDirectory |> ignore
//    for (sample, testCase) in testCasesTuple do
//        try
//            let assemblyRefs = getRuntimeRefs
//            testCase.Dump (resolutionFolder, expectedDirectory, net45RuntimeAssembly, assemblyRefs, signatureOnly=false, ignoreOutput=false)
//            |> ignore
//        with e ->
//            raise(new Exception(sprintf "Failed generating: %s" sample, e))

