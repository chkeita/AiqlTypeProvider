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
    member val test = 0 with get,set

module Tests =

    type Trace= 
        abstract timestamp : DateTime
        abstract operation_name : string

    type Tables () =
        static member requests = Unchecked.defaultof<Requests[]>


        
    type ExpressionTest(output:ITestOutputHelper) =

            member x.sendQuery<'T> q = 
                async {
                    let query = toAiql q
                    sprintf "sending Query: %s" query
                    |> output.WriteLine 
                    return! ExpressionBuilder.ResultParer.sendRequest<'T>("https://api.applicationinsights.io/v1/apps/DEMO_APP", "DEMO_KEY") query
                }
                |> Async.RunSynchronously
            
           
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