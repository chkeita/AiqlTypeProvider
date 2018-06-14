// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open AiqlTypeProvider

type appinsight = AzureQueryTypeProvider.ApplicationInsights<address="demo", api_key="demo">

[<EntryPoint>]
let main argv = 
    //let context = appinsight.Context.QueryData
    let x = appinsight.Context.QueryData(appinsight.requests |> ExpressionBuilder.Expression.take 10) |> Async.RunSynchronously
    let head = Seq.head x

    printfn "%O" head.appId
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
