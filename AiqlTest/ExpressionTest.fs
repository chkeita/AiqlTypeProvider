namespace AiqlTest

open Xunit
open Xunit.Abstractions
open System.Net.Http
open System.Web
open System
open ExpressionBuilder.Expression

type Result<'T> = OK of 'T | Error of string

type Requests() =
    member val resultCode = 0 with get,set
    //member val test = 0 with get,set
    override x.ToString() = sprintf "resultCode = %d" x.resultCode

type TestRecord = {
        ResultCode: int
        TestField: string
    }

module Tests =

    type Trace= 
        abstract timestamp : DateTime
        abstract operation_name : string

    type Tables () =
        static member requests = Unchecked.defaultof<Requests[]>

    type ExpressionTest(output:ITestOutputHelper) =
    
        let assertAiql expected actual =
            let query = toAiql actual
            sprintf "sending Query: %s" query
            |> output.WriteLine 
            Assert.Equal(expected, query)

        member x.sendQuery<'T> q = 
            let result = 
                async {
                    let query = toAiql q
                    sprintf "sending Query: %s" query
                    |> output.WriteLine 
                    return! ExpressionBuilder.ResultParer.sendRequest<'T>("https://api.applicationinsights.io/v1/apps/DEMO_APP", "DEMO_KEY") query
                }
                |> Async.RunSynchronously
                |> Seq.toList

            output.WriteLine(sprintf "query result:\n [%s]" (result |> Seq.map (sprintf "%O") |> String.concat ";"))
            result
           
        [<Fact>]
        member x.whereExpr () =
            <@
                Tables.requests |> where (fun x -> x.resultCode = 1)
            @>
            |> assertAiql "requests | where resultCode == 1"

        [<Fact>]
        member x.whereExprLet () = 
            <@
                let x = 1
                Tables.requests |> where (fun s -> s.resultCode = x)
            @>
            |> assertAiql "let x = 1;\nrequests | where resultCode == x"

        [<Fact>]
        member x.whereExprLetLambda () = 
            <@
                let add (x,y) = x+y
                Tables.requests |> where (fun s -> s.resultCode = add(1,2))
            @>
            |> assertAiql "let add = (x:int, y:int) { x + y };\nrequests | where resultCode == add(1, 2)"

        [<Fact>]
        member x.takeExpression () = 
            <@
                Tables.requests |> take 10
            @>
            |> assertAiql "requests | take 10"

        [<Fact>]
        member x.``Project- mapping to record intialized in the same order as the defnition`` () = 
            <@
                Tables.requests |> project (fun r ->  {ResultCode = r.resultCode; TestField = "" })
            @>
            |> assertAiql "requests | project ResultCode = resultCode, TestField = \"\""

        [<Fact>]
        member x.``Project- mapping to record intialized in an order different from the defnition`` () = 
            <@
                Tables.requests |> project (fun r ->  {TestField = "" ; ResultCode = r.resultCode})
            @>
            |> assertAiql "requests | project ResultCode = resultCode, TestField = \"\""
                

            //[<Fact>]
            //member x.orderByExpression () = 
            //    <@
            //        Tables.requests |> orderBy (fun x -> x.resultCode)
            //    @>
            //    |> x.sendQuery<Requests>


            //[<Fact>]
            //member x.projectExpression () = 
            //    <@
            //        Tables.requests |> project (fun x -> x.resultCode, x.test)
            //    @>
            //    |> x.sendQuery<Requests>