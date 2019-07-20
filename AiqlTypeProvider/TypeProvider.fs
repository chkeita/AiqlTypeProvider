namespace AiqlTypeProvider

open Microsoft.FSharp
open Microsoft.FSharp.Core.CompilerServices
open System
open ExpressionBuilder
open ExpressionBuilder.Expression
open ExpressionBuilder.ResultParer
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.Extensions
open FSharp.Control

type ApplicationInsightsBase() = class end

type ApplicationInsightsContext (address:string, apiKey:string) =
    member this.QueryData<'T> ([<ReflectedDefinition>] q: Quotations.Expr<seq<'T>>) : AsyncSeq<'T> =
        q
        |> Expression.toAiqlQuery
        |> ExpressionWriter.fromAiqlQuery
        |> sendRequest (address, apiKey)

type SourceStream() =
    class end

//[<TypeProvider>]
type AiqlTypeProvider (config : TypeProviderConfig) as this =
    
    inherit TypeProviderForNamespaces (config, addDefaultProbingLocation=true)

    let ns = "AzureQueryTypeProvider"
    let asm = ProvidedAssembly()

    let mainType = ProvidedTypeDefinition(asm, ns, "ApplicationInsights",  Some typeof<ApplicationInsightsBase>, isErased=false)
    let createTypes typeName address key=
        let schema = getSchema ("https://api.applicationinsights.io/V1/apps/DEMO_APP", "DEMO_KEY") |> Async.RunSynchronously
        let tableType = ProvidedTypeDefinition(asm, ns, typeName,  Some typeof<ApplicationInsightsBase>, isErased=false)
        let convertType typeName =
            let typ = 
                match typeName with
                | "timestamp"
                | "ingestionTime" -> typeof<DateTimeOffset>
                | "System.Int32" -> typeof<Int64> 
                | ty -> Type.GetType ty

            if isNull typ then
                failwithf "Unknown column type: %s" typeName
            else
                typ

        let tableTypes =
            schema.Tables
            |> Seq.collect(fun x -> x.Rows)
            |> Seq.map(
                function
                | [|tableName; columnName; typeName |] -> (tableName, columnName, typeName, convertType typeName)
                | _ -> invalidOp "the row contains the wrong number of columns"
                )
            |> Seq.groupBy (fun (tableName, _, _, _) -> tableName )
            |> Seq.map(fun (key, grp) ->
                let recordFields = grp |> Seq.map(fun (_, columnName, _, columnType) -> columnName, columnType )
                ProvidedRecordDefnition(asm, ns, key, recordFields)
            )
            |> Seq.toList

        let contextProperty = ProvidedProperty(
                                propertyName = "Context",
                                propertyType = typeof<ApplicationInsightsContext>,
                                getterCode = (fun _ -> <@@ ApplicationInsightsContext(%%(Quotations.Expr.Value address), %%(Quotations.Expr.Value key))  @@> ),
                                isStatic = true)


        tableType.AddMember contextProperty
        for typedef in tableTypes do
            let seqOfType = typeof<seq<_>>.GetGenericTypeDefinition().MakeGenericType(typedef)
            tableType.AddMember typedef
            tableType.AddMember <| ProvidedProperty(typedef.Name, seqOfType, getterCode = (fun args -> <@@ getTable<_> %(Quotations.Expr.Value(typedef.Name.ToLowerInvariant()) |> Quotations.Expr.Cast) @@>), isStatic = true)

        asm.AddTypes ([mainType;tableType])

        tableType

    let staticParams = [ProvidedStaticParameter("address", typeof<string>); ProvidedStaticParameter("api_key", typeof<string>)]

    do mainType.DefineStaticParameters(
        parameters=staticParams,
        instantiationFunction=(fun typeName ->
            function
            | [| :? string as address; :? string as apiKey |] ->
                match (address,apiKey) with
                | ("demo",_) | (_,"demo") ->
                    
                    createTypes typeName "https://api.applicationinsights.io/V1/apps/DEMO_APP" "DEMO_KEY"
                | _ ->
                    
                    createTypes typeName address apiKey
            | _ -> failwith "unexpected parameter values"))

    do this.AddNamespace(ns, [mainType])

[<assembly:TypeProviderAssembly>]
do ()