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
                |> x.sendQuery<Requests>
                

            [<Fact>]
            member x.whereExprLet () = 
                <@
                    let x = 1
                    Tables.requests |> where (fun s -> s.resultCode = x)
                @>
                |> x.sendQuery<Requests>


            [<Fact>]
            member x.whereExprLetLambda () = 
                <@
                    let x = 1
                    Tables.requests |> where (fun s -> s.resultCode = x)
                @>
                |> x.sendQuery<Requests>

            [<Fact>]
            member x.takeExpression () = 
                let req = 
                    <@
                        Tables.requests |> take 10
                    @>
                    |> x.sendQuery<Requests>
                Xunit.Assert.Equal(10, req |> Seq.length)

            [<Fact>]
            member x.``Project- mapping to record intialized in the same order as the defnition`` () = 
                <@
                    Tables.requests |> project (fun r ->  {ResultCode = r.resultCode; TestField = "" })
                @>
                |> x.sendQuery<Requests>

            [<Fact>]
            member x.``Project- mapping to record intialized in an order different from the defnition`` () = 
                <@
                    Tables.requests |> project (fun r ->  {TestField = "" ; ResultCode = r.resultCode})
                @>
                |> x.sendQuery<Requests>
                

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