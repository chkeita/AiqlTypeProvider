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

    let ns = "Sample.AiqlTypeProvider"
    let asm = Assembly.GetExecutingAssembly()

    let mainType = ProvidedTypeDefinition(asm, ns, "Tables",  Some typeof<obj>)
    let createTypes (address:string) (key:string) =
        use client = new HttpClient()
        client.DefaultRequestHeaders.Add("x-api-key",key)
        let result = 
            client.GetAsync(address).Result.Content.ReadAsStringAsync().Result

        let tableData = JsonConvert.DeserializeObject<TableResult>(result)
        
        let tableTypes = 
            tableData.Tables
            |> Seq.collect(fun x -> x.Rows)
            |> Seq.map(fun x -> x.[0], x.[1], Type.GetType( x.[2]))
            |> Seq.groupBy (fun (tableName, _, _) -> tableName )
            |> Seq.map(fun (key, grp) -> 
                                let myType = ProvidedTypeDefinition(asm, ns, key,  Some typeof<obj>)
                                grp
                                |> Seq.iter (fun (_, columnName, columnType) -> 
                                    myType.AddMember <| ProvidedProperty(columnName, columnType, GetterCode = (fun args -> <@@ NotImplementedException "" |> raise @@>)))
                                myType
                                )
            |> Seq.iter(fun typedef -> 
                mainType.AddMember <| ProvidedProperty(typedef.Name, typedef.DeclaringType, GetterCode = (fun args -> <@@ getTable<_> %(Quotations.Expr.Value(typedef.Name) |> Quotations.Expr.Cast) @@>))
            )
        mainType

    let staticParams = [ProvidedStaticParameter("address", typeof<string>); ProvidedStaticParameter("api_key", typeof<string>)]
        
    do mainType.DefineStaticParameters(
        parameters=staticParams,
        instantiationFunction=(fun typeName ->
            function 
            | [| :? string as address; :? string as apiKey |] -> 
                createTypes "https://api.applicationinsights.io/beta/apps/DEMO_APP/query/schema" "DEMO_KEY"
                    
            | _ -> failwith "unexpected parameter values")) 
                
                
        
        //this.

        //do regexTy.DefineStaticParameters(
        //parameters=c, 
        //instantiationFunction=(fun typeName parameterValues ->
                                    

    do this.AddNamespace(ns, [mainType])

[<assembly:TypeProviderAssembly>]
do ()