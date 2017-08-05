namespace AiqlTest

open Xunit
open Xunit.Abstractions
open System.Net.Http
open System.Web
open System
open ExpressionBuilder.Expression

type Result<'T> = OK of 'T | Error of string

type Requests =
    abstract resultCode:int

module Tests =
    type Trace= 
        abstract timestamp : DateTime
        abstract operation_name : string
        //static member QueryData (expr:Quotations.Expr<Trace -> 'a>):'a
    

    type Tables =
        abstract requests : Requests[]

    type ExpressionTest(output:ITestOutputHelper) = 
            let sendQuery (query:string) = 
                async{
                    sprintf "sending Query: %s" query
                    |> output.WriteLine 
                    use client = new HttpClient()
                    client.DefaultRequestHeaders.Add("x-api-key","DEMO_KEY")
                    let! result = 
                        client.GetAsync(sprintf "https://api.applicationinsights.io/beta/apps/DEMO_APP/query?query=%s" (HttpUtility.UrlEncode query))
                        |> Async.AwaitTask

                    let! r = result.Content.ReadAsStringAsync() |> Async.AwaitTask
                    if result.IsSuccessStatusCode then
                        return OK r
                    else 
                        return Error r
                }

            let testQuery q = async {
                let! r = 
                        q
                        |> toAiql
                        |> sendQuery

                match r with 
                | OK _ -> ()
                | Error mes -> Exception mes |> raise
            }
           
            [<Fact>]
            let whereExpr () = 
                <@
                    getTable<Requests[]> "requests" |> where (fun x -> x.resultCode = 1)
                @>
                |> testQuery

            [<Fact>]
            let whereExprLet () = 
                <@
                    let x = 1
                    getTable<Requests[]> "requests" |> where (fun s -> s.resultCode = x)
                @>
                |> testQuery


            [<Fact>]
            let whereExprLetLambda () = 
                <@
                    fun (tables:Tables) -> 
                        let x = 1
                        tables.requests |> where (fun s -> s.resultCode = x)
                @>
                |> testQuery
