//#r @"C:\Users\keita\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\netstandard.dll"
#r "netstandard"
#r "System.Runtime.Serialization"
#r "System.Net.Http"
#r @"C:\Users\keita\.nuget\packages\newtonsoft.json\12.0.1\lib\netstandard2.0\Newtonsoft.Json.dll"
#load @"..\Contract.fs"
#load @"..\AiqlResultParser.fs"
#load @"..\ExpressionBuilder.fs"
#load @"..\ExpressionWriter.fs"


open ExpressionBuilder
open System
open Microsoft.FSharp.Reflection

type Request () =
    member val timestamp = Unchecked.defaultof<DateTime> with get, set
    member val id = Unchecked.defaultof<string> with get, set
    member val source = Unchecked.defaultof<string> with get, set
    member val name = Unchecked.defaultof<string> with get, set
    member val url = Unchecked.defaultof<string> with get, set
    member val success = Unchecked.defaultof<string> with get, set
    member val resultCode = Unchecked.defaultof<string> with get, set
    member val duration = Unchecked.defaultof<float> with get, set

//let logIt message x =
//    printfn "%s" message
//    printfn "%A" x
//    x


//let convertExpression  = 
//    q
//    |> Expression.toAiqlQuery 
//    |> logIt "Expression"
//    |> ExpressionWriter.fromAiqlQuery
//    |> logIt "Qie"


let fsharpExpression = 
    <@ 
        (Expression.getTable<Request>("Request"), Expression.getTable<Request>("Request"))
        |> Expression.join( fun (left,right) -> left.name = right.name ) 
    @>

let aiqlExpression = Expression.toAiqlQuery fsharpExpression
let aiqlString = ExpressionWriter.fromAiqlQuery aiqlExpression


let (|TupledArgMethodCallPattern|_|) expr =
    let rec isMethodCall args expr =
        match expr with
        | Quotations.ExprShape.ShapeLambda (_, z) -> 
            isMethodCall args z
        | Quotations.Patterns.Call (_,methodInfo,b) ->
            b
            |> Seq.iter (printfn "#### b %A")
            
            printfn "#### args %A" args
            Some (methodInfo,args)
        | Quotations.Patterns.Let (arg, body, remaining) ->
            isMethodCall (Map.add arg.Name body args) remaining
        | _ -> None
    isMethodCall Map.empty expr


match 
     <@ 
        fun (left:Request, right:Request) -> left.name = right.name
    @>  with
| Quotations.DerivedPatterns.Lambdas (args,Quotations.Patterns.Call(None, operator, [Quotations.Patterns.PropertyGet(Some (Quotations.Patterns.Var ob1), propinfo1,_); Quotations.Patterns.PropertyGet(Some ob2, propinfo2,_) ]))->

    printfn "ob1 %A"  ob1
    printfn "propinfo1 %A"  propinfo1
    printfn "ob2 %A"  ob2
    printfn "propinfo2 %A"  propinfo2
| _ -> printfn "nope"

match 
    //<@ 
    //      (Expression.getTable<Request>("Request") , Expression.getTable<Request>("Request") )
    //      |> Expression.join2( fun joined -> joined.Left.name = joined.Right.name ) 
    //@> 
    <@ 
        (Expression.getTable<Request>("Request") , Expression.getTable<Request>("Request") )
        |> Expression.join( fun (left, right) -> left.name = right.name ) 
    @> 
    //<@ 
    //    Expression.getTable<Request>("Request") 
    //    |> Expression.where(fun x -> x.name = "blah")
    //@>
    
    with
| Quotations.DerivedPatterns.SpecificCall
           <@ (|>) @>
           (_, _, [
               left
               right
               ]) ->
    match right with 
    
    | Quotations.ExprShape.ShapeCombination (
        _,
        [
            firstArg
            Quotations.DerivedPatterns.Lambdas (_, lambdaBody) as secondArg
        ]) ->
        printfn "***leftArg ** %A" left
        printfn "***first arg ** %A" firstArg
        printfn "***second arg ** %A" secondArg
        match lambdaBody with 
        | Quotations.ExprShape.ShapeCombination y -> 
            printfn "### shape combo %A"  y
        | TupledArgMethodCallPattern y -> 
            
            printfn "***shape of shape** %A" y
    | Quotations.ExprShape.ShapeCombination y -> 
        printfn "**ShapeCombination*** %A" y
| Quotations.DerivedPatterns.SpecificCall <@ (|>) @> x -> 
    printfn "%A" x 



<@ 
Expression.getTable<Request>("Request") 
|> Expression.where(fun x -> x.name = "blah")
@>