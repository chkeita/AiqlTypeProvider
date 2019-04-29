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
    member val name = "" with get,set
    override x.ToString() = sprintf "resultCode = %d" x.resultCode

type TestRecord = {
        ResultCode: int
        TestField: string
    }

module Tests =
    open ExpressionBuilder

    type Trace= 
        abstract timestamp : DateTime
        abstract operation_name : string

    type Tables () =
        static member requests = Unchecked.defaultof<Requests[]>

    let ``Tables.requests |> where (fun x -> x.resultCode = 1)`` =
        [
            AiqlExpression( 
                TabularExpression(
                    [Table "requests"], 
                    Where, 
                    BinaryOperation( 
                        PropertyGet ("x", "resultCode"), 
                        ConstantExpression 1, 
                        Equal  
                    ) 
                )
            ) 
        ]

    let ``let x = 1; Tables.requests |> where (fun s -> s.resultCode = x)`` =
        [
            LetBinding("x", ConstantExpression 1)
            AiqlExpression( 
                TabularExpression(
                    [Table "requests"], 
                    Where, 
                    BinaryOperation( 
                        PropertyGet ("s", "resultCode"), 
                        Var "x", 
                        Equal  
                    ) 
                )
            ) 
        ]

    let ``let add (x,y) = x+y; Tables.requests |> where (fun s -> s.resultCode = add(1,2))``=
        [
            LetBinding(
                "add", 
                Lambda(
                    [| "x", AiqlType.Int; "y", AiqlType.Int |],
                    BinaryOperation(Var "x", Var "y",Plus) 
                )
            )
            AiqlExpression( 
                TabularExpression(
                    [Table "requests"], 
                    Where, 
                    BinaryOperation( 
                        PropertyGet ("s", "resultCode"), 
                        FunctionAppliation (
                            "add", 
                            [ConstantExpression 1; ConstantExpression 2]), 
                        Equal  
                    ) 
                )
            ) 
        ]

    let ``Tables.requests |> take 10`` =
        [
            AiqlExpression( 
                TabularExpression(
                    [Table "requests"], 
                    Take, 
                    ConstantExpression 10
                )
            ) 
        ]

    let ``Tables.requests |> project (fun r ->  {ResultCode = r.resultCode; TestField = "" })`` =
        [
            AiqlExpression( 
                TabularExpression(
                    [Table "requests"], 
                    Project,
                    PropPertyList
                        [
                            "ResultCode", PropertyGet ("r", "resultCode")
                            "TestField", ConstantExpression ""
                        ]
                )
            ) 
        ]

    let ``Tables.requests |> project (fun r ->  {TestField = "" ; ResultCode = r.resultCode})``=
        [
            AiqlExpression( 
                TabularExpression(
                    [Table "requests"], 
                    Project,
                    PropPertyList
                        [
                            "ResultCode", PropertyGet ("r", "resultCode")
                            "TestField", ConstantExpression ""
                        ]
                )
            ) 
        ]

    let ``Request | join (Request) ON  $left.name == $right.name`` =
        [
            AiqlExpression(
                TabularExpression(
                    [Table "requests"; Table "requests"],
                    Join,
                    BinaryOperation(
                        PropertyGet ("$left","name"),
                        PropertyGet ("$right","name"),
                        Equal
                    )
                )
            )
        ]
        

    type ExpressionBuildingTest(output:ITestOutputHelper) =
    
        let assertAiql (expected:#seq<AiqlStatement>) (actual) =
            let query = 
                actual
                |> Expression.toAiqlQuery 
            Assert.Equal(expected, query)

        [<Fact>]
        member x.whereExpr () =
            <@
                Tables.requests |> where (fun x -> x.resultCode = 1)
            @>
            |> assertAiql ``Tables.requests |> where (fun x -> x.resultCode = 1)``

        [<Fact>]
        member x.whereExprLet () = 
            <@
                let x = 1
                Tables.requests |> where (fun s -> s.resultCode = x)
            @>
            |> assertAiql ``let x = 1; Tables.requests |> where (fun s -> s.resultCode = x)``

        [<Fact>]
        member x.whereExprLetLambda () = 
            <@
                let add (x,y) = x+y
                Tables.requests |> where (fun s -> s.resultCode = add(1,2))
            @>
            |> assertAiql ``let add (x,y) = x+y; Tables.requests |> where (fun s -> s.resultCode = add(1,2))``
            

        [<Fact>]
        member x.takeExpression () = 
            <@
                Tables.requests |> take 10
            @>
            |> assertAiql ``Tables.requests |> take 10``
            
        [<Fact>]
        member x.``Project - mapping to record intialized in the same order as the defnition`` () = 
            <@
                Tables.requests |> project (fun r ->  {ResultCode = r.resultCode; TestField = "" })
            @>
            |> assertAiql ``Tables.requests |> project (fun r ->  {ResultCode = r.resultCode; TestField = "" })``

        [<Fact>]
        member x.``Project - mapping to anonymous record`` () = 
            <@
                Tables.requests |> project (fun r ->  {| ResultCode = r.resultCode; TestField = "" |})
            @>
            |> assertAiql ``Tables.requests |> project (fun r ->  {ResultCode = r.resultCode; TestField = "" })``
        
        [<Fact>]
        member x.``Project - mapping to record intialized in an order different from the defnition`` () = 
            <@
                Tables.requests |> project (fun r ->  {TestField = "" ; ResultCode = r.resultCode})
            @>

            |> assertAiql ``Tables.requests |> project (fun r ->  {TestField = "" ; ResultCode = r.resultCode})``

        [<Fact>]
        member x.joinExpression() = 
            <@ 
                   (Tables.requests, Tables.requests)
                   |> Expression.join( fun (left,right) -> left.name = right.name ) 
            @>
            |> assertAiql ``Request | join (Request) ON  $left.name == $right.name``

    
    type ExpressionWriterTest(output:ITestOutputHelper) =
        let assertAiql expected actual =
            let query = 
                actual
                |> ExpressionWriter.fromAiqlQuery
            sprintf "sending Query: %s" query
            |> output.WriteLine 
            Assert.Equal(expected, query, System.StringComparer.OrdinalIgnoreCase)

        [<Fact>]
        member x.whereExpr () =
            ``Tables.requests |> where (fun x -> x.resultCode = 1)``
            |> assertAiql "requests | where resultCode == 1"

        [<Fact>]
        member x.whereExprLet () = 
            ``let x = 1; Tables.requests |> where (fun s -> s.resultCode = x)``
            |> assertAiql "let x = 1;\nrequests | where resultCode == x"

        [<Fact>]
        member x.whereExprLetLambda () = 
            ``let add (x,y) = x+y; Tables.requests |> where (fun s -> s.resultCode = add(1,2))``
            |> assertAiql "let add = (x:int, y:int) { x + y };\nrequests | where resultCode == add(1, 2)"

        [<Fact>]
        member x.takeExpression () = 
            ``Tables.requests |> take 10``
            |> assertAiql "requests | take 10"

        [<Fact>]
        member x.``Project - mapping to record intialized in the same order as the defnition`` () = 
            ``Tables.requests |> project (fun r ->  {ResultCode = r.resultCode; TestField = "" })``
            |> assertAiql "requests | project ResultCode = resultCode, TestField = \"\""

        [<Fact>]
        member x.``Project - mapping to record intialized in an order different from the defnition`` () = 
            ``Tables.requests |> project (fun r ->  {TestField = "" ; ResultCode = r.resultCode})``
            |> assertAiql "requests | project ResultCode = resultCode, TestField = \"\""

        [<Fact>]
        member x.joinExpression() = 
            ``Request | join (Request) ON  $left.name == $right.name``
            |> assertAiql "requests | join (requests) ON  $left.name == $right.name"
                

        