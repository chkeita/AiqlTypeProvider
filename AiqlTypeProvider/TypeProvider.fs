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

    let mainType = ProvidedTypeDefinition(asm, ns, "Tables",  Some typeof<obj>)
    let createTypes typeName (address:string) (key:string) =
        use client = new HttpClient()
        client.DefaultRequestHeaders.Add("x-api-key",key)
        let result = 
            client.GetAsync(address).Result.Content.ReadAsStringAsync().Result

        let tableData = JsonConvert.DeserializeObject<TableResult>(result)
        let tableType = ProvidedTypeDefinition(asm, ns, typeName,  Some typeof<obj>)
        let firstLetterToUpper (s:string) =
            sprintf "%c%s" (Char.ToUpper(s.[0])) (s.Substring(1,s.Length-1))
            
        let tableTypes = 
            tableData.Tables
            |> Seq.collect(fun x -> x.Rows)
            |> Seq.map(fun x -> x.[0], x.[1], Type.GetType( x.[2]))
            |> Seq.groupBy (fun (tableName, _, _) -> tableName )
            |> Seq.map(fun (key, grp) -> 
                                let myType = ProvidedTypeDefinition(firstLetterToUpper key,  Some typeof<obj>)
                                for (_, columnName, columnType) in grp do 
                                    myType.AddMember <| ProvidedProperty(firstLetterToUpper columnName, columnType, GetterCode = (fun args -> <@@ NotImplementedException "" |> raise @@>))
                                myType
                                )
            |> Seq.toList
            
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