namespace AzureQueryTypeProvider

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Reflection
open System.Net.Http
open Newtonsoft.Json
open ExpressionBuilder.Expression
open System.Web

type ApplicationInsightsBase() = class end

type ApplicationInsightsBase<'T> () =
    inherit ApplicationInsightsBase()
    static member QueryData (address, apiKey, [<ReflectedDefinition>] q: Quotations.Expr<'T -> _>) =
        
        let query = 
            match q with 
            | Quotations.Patterns.Lambda (vars, body) -> 
                ExpressionBuilder.Expression.toAiql body
            | _ -> failwith (sprintf "Unexpected query: %O" q)

        async {
            use client = new HttpClient()
            client.DefaultRequestHeaders.Add("x-api-key",sprintf "%s" apiKey)
            let! result = 
                client.GetAsync(sprintf "%s/query?query=%s" address (HttpUtility.UrlEncode query))
                |> Async.AwaitTask

            let! resultText = result.Content.ReadAsStringAsync() |> Async.AwaitTask
            if result.IsSuccessStatusCode then
                return resultText
            else
                return failwith (sprintf "Failed query, Status code: %d;\n%s" (int result.StatusCode) resultText)

        }

type SourceStream = class end


namespace AiqlTypeProvider

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Reflection
open System.Net.Http
open Newtonsoft.Json
open ExpressionBuilder.Expression

type ColumnDefinition = {
    ColumnName: string
    DataType: string
    ColumnType: string
}

type TableData = {
    TableName: string
    Columns: ColumnDefinition[]
    Rows: string[][]
}

type TableResult = {
   Tables : TableData[] 
}

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
            client.GetAsync(address).Result.Content.ReadAsStringAsync().Result

        let tableData = JsonConvert.DeserializeObject<TableResult>(result)
        let tableType = ProvidedTypeDefinition(asm, ns, typeName,  Some typeof<AzureQueryTypeProvider.ApplicationInsightsBase>)
            
        let tableTypes = 
            tableData.Tables
            |> Seq.collect(fun x -> x.Rows)
            |> Seq.map(fun x -> x.[0], x.[1], Type.GetType( x.[2]))
            |> Seq.groupBy (fun (tableName, _, _) -> tableName )
            |> Seq.map(fun (key, grp) -> 
                let myType = ProvidedTypeDefinition(key,  Some typeof<AzureQueryTypeProvider.SourceStream>)
                for (_, columnName, columnType) in grp do 
                    myType.AddMember <| ProvidedProperty(columnName, columnType, GetterCode = (fun args -> <@@ NotImplementedException "" |> raise @@>))
                myType

            )
            |> Seq.toList

        // adding static member QueryData (expr:Quotations.Expr<Trace -> 'a>):'a
        //let queryDataMethod = ProvidedMethod(methodName = "QueryData", parameters  = [], returnType = typeof<obj>)
        //queryDataMethod
            
        for typedef in tableTypes do 
            tableType.AddMember typedef
            tableType.AddMember <| ProvidedProperty(typedef.Name, typedef, GetterCode = (fun args -> <@@ getTable<_> %(Quotations.Expr.Value(typedef.Name.ToLowerInvariant()) |> Quotations.Expr.Cast) @@>), IsStatic = true)
        
        tableType

    let staticParams = [ProvidedStaticParameter("address", typeof<string>); ProvidedStaticParameter("api_key", typeof<string>)]
        
    do mainType.DefineStaticParameters(
        parameters=staticParams,
        instantiationFunction=(fun typeName ->
            function 
            | [| :? string as address; :? string as apiKey |] -> 
                match (address,apiKey) with
                | ("demo",_) | (_,"demo") -> 
                    createTypes typeName "https://api.applicationinsights.io/beta/apps/DEMO_APP/query/schema" "DEMO_KEY"
                | _ -> 
                    createTypes typeName address apiKey
            | _ -> failwith "unexpected parameter values")) 
                

    do this.AddNamespace(ns, [mainType])

[<assembly:TypeProviderAssembly>]
do ()