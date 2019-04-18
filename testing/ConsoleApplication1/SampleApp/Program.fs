// Learn more about F# at http://fsharp.org

open System
open AiqlTypeProvider


type appinsight = AzureQueryTypeProvider.ApplicationInsights<address="demo", api_key="demo">

[<EntryPoint>]
let main argv = 
    //let context = appinsight.Context.QueryData
    let x = appinsight.Context.QueryData(
                appinsight.requests 
                |> ExpressionBuilder.Expression.take 10
                |> ExpressionBuilder.Expression.project (fun r ->  {| AppId = r.appId; Client_Browser = r.client_Browser |}) 
            ) |> Async.RunSynchronously
    let head = Seq.head x

    printfn "%O" head
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
