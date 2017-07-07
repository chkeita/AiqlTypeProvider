namespace AiqlTypeProvider

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Reflection
open System.Net.Http
open Newtonsoft.Json

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

    let ns = "AiqlTypeProvider"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        use client = new HttpClient()
        client.DefaultRequestHeaders.Add("x-api-key","DEMO_KEY")
        let result = 
            client.GetAsync("https://api.applicationinsights.io/beta/apps/DEMO_APP/query/schema").Result.Content.ReadAsStringAsync().Result

        let tableData =
            JsonConvert.DeserializeObject<TableResult>(result)
    
        tableData.Tables
        |> Seq.collect(fun x -> x.Rows)
        |> Seq.map(fun x -> x.[0], x.[1], Type.GetType( x.[2]))
        |> Seq.groupBy (fun (tableName, _, _) -> tableName )
        |> Seq.map(fun (key, grp) -> 
                            let myType = ProvidedTypeDefinition(key,  Some typeof<obj>)
                            grp
                            |> Seq.iter (fun (_, columnName, columnType) -> myType.AddMember <| ProvidedProperty(columnName, columnType, IsStatic = true))
                            myType
                            )
        |> Seq.toList
                                    

    do
        this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()