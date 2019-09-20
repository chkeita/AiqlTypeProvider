// Learn more about F# at http://fsharp.org

open System
open AiqlTypeProvider
open FSharp.Control


//type appinsight = AzureQueryTypeProvider.ApplicationInsights<address="demo", api_key="demo">

//[<EntryPoint>]
//let main argv = 
//    //let context = appinsight.Context.QueryData
//    let x = appinsight.Context.QueryData(
//                appinsight.requests 

//                |> ExpressionBuilder.Expression.take 10
//                //|> ExpressionBuilder.Expression.project (fun r ->  {| AppId = r.appId; Client_Browser = r.client_Browser |}) 
//            ) |> AsyncSeq.toBlockingSeq
//    let head = Seq.head x

//    printfn "%A" head
//    System.Console.ReadKey() |> ignore
//    0 // return an integer exit code

type TestRecord = {
    p1: int
    p2: int
}

type TestRecord2() = 
    member __.p1 with get() = 1
    member __.p2 with get() = 1


type public TestType = TestProvider.TestMainType<blah="demo">

let testRecordType () =
    FSharp.Reflection.FSharpType.IsRecord(typeof<TestType>)
    |> printfn "%A" 
    //=> true

    typeof<TestType>
        //.GetProperties()
        .DeclaringType
    //|> Seq.map(fun p -> p.DeclaringType)
    |> printfn "TestType: %A" 

    typeof<TestRecord>.GetProperties(Reflection.BindingFlags.Public)
    |> printfn "TestRecord: %A" 

    typeof<TestRecord>.CustomAttributes
    |> printfn "%A" 

    let test1 = TestType("", 1)
    printfn "%A" test1 
    //=> {prop1 = "";
    //=> prop2 = 1;}

    // This line does not compile 
    // error : The field, constructor or member 'prop1' is not defined
    

    let rec1 = {TestType.prop1 = ""; TestType.prop2 = 1}

    //let rec2 = {|rec1 with prop3 = true |}
    let rec3 ={TestRecord.p1 = 2; TestRecord.p2 = 2}
    rec1, rec1.prop1, rec3.p1

[<EntryPoint>]
let main argv =
    testRecordType()
    |> printfn "%A"

    //printfn "%A" (FSharp.Reflection.FSharpType.IsRecord(typeof<TestType>))
    //typeof<TestType>.GetMethods()
    //|> Seq.map(fun t -> sprintf "%O %A" t t.Attributes )
    //|> Seq.iter (printfn "%s")
    //let t = TestType("", 1)

    //{ t with prop1 = "dsadas"}
    //{prop1 = ""; prop2 = 1}
    
    //|> printfn "%A"

    //{TestType.prop1 = ""; TestType.prop2 = 1}
    0
    