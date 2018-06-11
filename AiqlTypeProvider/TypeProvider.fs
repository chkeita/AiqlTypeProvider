namespace AzureQueryTypeProvider

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Reflection
open System.Net.Http
open Newtonsoft.Json
open ExpressionBuilder.Expression
open System.Web
open AiqlContract.ResultParer

type ApplicationInsightsBase() = class end

type ApplicationInsightsContext (address:string, apiKey:string) =
    member this.QueryData<'T> ([<ReflectedDefinition>] q: Quotations.Expr<seq<'T>>) : Async<seq<'T>> =
        
        let query = ExpressionBuilder.Expression.toAiql q
        
        async {
            use client = new HttpClient()
            client.DefaultRequestHeaders.Add("x-api-key",sprintf "%s" apiKey)
            let! result = 
                client.GetAsync(sprintf "%s?query=%s" address (HttpUtility.UrlEncode query))
                |> Async.AwaitTask

            if result.IsSuccessStatusCode then
                let! resultStream = result.Content.ReadAsStreamAsync() |> Async.AwaitTask
                return 
                    seq { 
                        for r in readResults(q.Type, resultStream) do 
                            yield r :?> 'T 
                    } 
            else
                let! resultText = result.Content.ReadAsStringAsync() |> Async.AwaitTask
                return failwith (sprintf "Failed query, Status code: %d;\n%s" (int result.StatusCode) resultText)

        }

type SourceStream() = 
    class end
namespace AiqlTypeProvider

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Reflection
open System.Net.Http
open Newtonsoft.Json
open ExpressionBuilder.Expression
open AiqlContract

[<TypeProvider>]
type AiqlTypeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "AzureQueryTypeProvider"
    let asm = Assembly.GetExecutingAssembly()

    let mainType = ProvidedTypeDefinition(asm, ns, "ApplicationInsights",  Some typeof<AzureQueryTypeProvider.ApplicationInsightsBase>)
    let createTypes typeName (address:string) (key:string) =
        use client = new HttpClient()
        client.DefaultRequestHeaders.Add("x-api-key",key)
        let result = 
            client.GetAsync(sprintf "%s/schema" address).Result.Content.ReadAsStringAsync().Result

        let tableData = JsonConvert.DeserializeObject<TableResult>(result)
        let tableType = ProvidedTypeDefinition(asm, ns, typeName,  Some typeof<AzureQueryTypeProvider.ApplicationInsightsBase>)
        let tableTypes = 
            tableData.Tables
            |> Seq.collect(fun x -> x.Rows)
            |> Seq.map(fun x -> x.[0], x.[1], Type.GetType( x.[2]))
            |> Seq.groupBy (fun (tableName, _, _) -> tableName )
            |> Seq.map(fun (key, grp) -> 
                //let myType = ProvidedTypeDefinition(key,  Some typeof<AzureQueryTypeProvider.SourceStream>)
                let myType = ProvidedTypeDefinition(key,  None)
                myType.AddMember <| ProvidedConstructor([])
                for (_, columnName, columnType) in grp do 
                    //myType.AddMember <| ProvidedProperty(columnName, columnType, GetterCode = (fun args -> <@@ NotImplementedException "" |> raise @@>))
                    let field = ProvidedField("_" + columnName, columnType)
                    myType.AddMember field
                    myType.AddMember( 
                        ProvidedProperty(
                            columnName, 
                            columnType,
                            GetterCode = (fun args ->  Quotations.Expr.FieldGet(args.[0], field)),
                            SetterCode = (fun args -> Quotations.Expr.FieldSet(args.[0], field, args.[1])))
                        )
                    
                    //property.
                    //property.
                    // todo: addsetter
                    // todo: add parameterless constructor to allow deserialization -- we do this for schema ??
                myType

            )
            |> Seq.toList

        // adding static member QueryData (expr:Quotations.Expr<Trace -> 'a>):'a
        let contextProperty = ProvidedProperty(
                                propertyName = "Context", 
                                propertyType = typeof<AzureQueryTypeProvider.ApplicationInsightsContext>,
                                GetterCode = (fun _ -> <@@ AzureQueryTypeProvider.ApplicationInsightsContext(%%(Quotations.Expr.Value address), %%(Quotations.Expr.Value key))  @@> ),
                                IsStatic = true)
        
        
        tableType.AddMember contextProperty
        for typedef in tableTypes do
            let seqOfType = typeof<seq<_>>.GetGenericTypeDefinition().MakeGenericType(typedef)
            tableType.AddMember typedef
            tableType.AddMember <| ProvidedProperty(typedef.Name, seqOfType, GetterCode = (fun args -> <@@ getTable<_> %(Quotations.Expr.Value(typedef.Name.ToLowerInvariant()) |> Quotations.Expr.Cast) @@>), IsStatic = true)
        
        tableType

    let staticParams = [ProvidedStaticParameter("address", typeof<string>); ProvidedStaticParameter("api_key", typeof<string>)]
        
    do mainType.DefineStaticParameters(
        parameters=staticParams,
        instantiationFunction=(fun typeName ->
            function 
            | [| :? string as address; :? string as apiKey |] -> 
                match (address,apiKey) with
                | ("demo",_) | (_,"demo") -> 
                    createTypes typeName "https://api.applicationinsights.io/V1/apps/DEMO_APP/query" "DEMO_KEY"
                | _ -> 
                    createTypes typeName address apiKey
            | _ -> failwith "unexpected parameter values")) 
                
    do this.AddNamespace(ns, [mainType])

[<assembly:TypeProviderAssembly>]
do ()