namespace AiqlTypeProvider

open Microsoft.FSharp

open Microsoft.FSharp.Core.CompilerServices
open System
open ExpressionBuilder.Expression
open ExpressionBuilder.ResultParer
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations

type ApplicationInsightsBase() = class end

type ApplicationInsightsContext (address:string, apiKey:string) =
    member this.QueryData<'T> ([<ReflectedDefinition>] q: Quotations.Expr<seq<'T>>) : Async<seq<'T>> =
        let query = ExpressionBuilder.Expression.toAiql q
        sendRequest (address, apiKey) query
       
type SourceStream() = 
    class end

[<TypeProvider>]
type AiqlTypeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "AzureQueryTypeProvider"

    let asm = ProvidedAssembly()

    let mainType = ProvidedTypeDefinition(asm, ns, "ApplicationInsights",  Some typeof<ApplicationInsightsBase>, isErased=false)
    let createTypes typeName (address:string) (key:string) =
        let tableData = getSchema (address, key) |> Async.RunSynchronously
        let tableType = ProvidedTypeDefinition(asm, ns, typeName,  Some typeof<ApplicationInsightsBase>, isErased=false)
        let convertType = 
            function
            | "timestamp"
            | "ingestionTime" -> typeof<DateTimeOffset>
            | ty -> Type.GetType ty

        let tableTypes = 
            tableData.Tables
            |> Seq.collect(fun x -> x.Rows)
            |> Seq.map(fun x -> x.[0], x.[1], x.[2] ,  convertType( x.[2]))
            |> Seq.groupBy (fun (tableName, _, _, _) -> tableName )
            |> Seq.map(fun (key, grp) -> 
                let myType = ProvidedTypeDefinition(key, Some typeof<SourceStream>, isErased=false)
                myType.AddMember <|  ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>)
                for (_, columnName, typeName, columnType) in grp do 
                    if isNull columnType then 
                        failwithf "Unknown column type: %s" typeName
                    let field = ProvidedField("_" + columnName, columnType)
                    myType.AddMember field
                    myType.AddMember( 
                        ProvidedProperty(
                            columnName, 
                            columnType,
                            getterCode = 
                                (fun args -> Quotations.Expr.FieldGetUnchecked(args.[0], field)),
                            setterCode = 
                                (fun args -> Quotations.Expr.FieldSetUnchecked(args.[0], field, args.[1])))
                        )
                    
                myType

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