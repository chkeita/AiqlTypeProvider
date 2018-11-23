# AIQL type provider

F# provider to query application analytics (work in progress)

## Usage 

```F#
open AiqlTypeProvider

// use demo as paramter to connect to the demo server
type appinsight = AzureQueryTypeProvider.ApplicationInsights<address="demo", api_key="demo">
let x = 
    appinsight.Context.QueryData(appinsight.requests 
    |> ExpressionBuilder.Expression.take 10) 
    |> Async.RunSynchronously
let head = Seq.head x

printfn "%O" head.appId
```