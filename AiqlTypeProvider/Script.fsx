// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "./Scripts/load-project-debug.fsx"
//open ExpressionBuilder

//type DataSource () = 
//    member x.Event:QueryExpresion<int[]> = { Expression = <@ getTable "Events" @>}
//    //member x.TestRec:QueryExpresion<TestRec[]> = { Expression = <@ getTable "testRec" @>}

//let ds = DataSource()

//let whereExpr =
//    ds.Event
//    |> QueryExpresion.Where (fun x -> x = 1)
    
//whereExpr.Expression 
//|> toAiql
//|> sprintf "%s"


// Define your library scripting code here

