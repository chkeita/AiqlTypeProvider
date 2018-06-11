// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open AzureQueryTypeProvider

type appinsight = AzureQueryTypeProvider.ApplicationInsights<"demo", "demo">

[<EntryPoint>]
let main argv = 
    let x = appinsight.Context.QueryData(appinsight.requests |> ExpressionBuilder.Expression.take 10) |> Async.RunSynchronously
    let head = Seq.head x

    let t = appinsight.requests.GetType()
    printfn "%O" "fsdsd"
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
