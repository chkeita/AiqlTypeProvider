module AiqlExpression

open System

let notImpl () = raise (NotImplementedException "Not ready")

type ScalarApplication =
| BooleanExpression
| FunctionExpression
with member this.AsString() = notImpl ()

type BooleanOperator =
| GraterThan
| LowerThan
| Equal
with member this.AsString() =
                match this with
                | GraterThan -> ">"
                | LowerThan  -> "<"
                | Equal      -> "=="

type BooleanExpression = { 
    left:Expression
    right:Expression
    operator:BooleanOperator
}
with member this.AsString() =  
                sprintf "%s %s %s" (this.left.AsString()) (this.operator.AsString()) (this.right.AsString())

and FunctionCall = 
| BoundFunctionCall of functionName:string*parameters:string[]
| BuiltinFunctionCall
| BooleanExpression of BooleanExpression
with member this.AsString() = notImpl ()

and Expression =
| String of string
| Number of int
| Boolean of bool
| PropertyReference of string
| FunctionCall of FunctionCall
with member this.AsString() = 
              match this with
                | String s -> sprintf "\"%s\"" s
                | Number n -> sprintf "%d" n
                | Boolean b -> sprintf "%O" box
                | PropertyReference ref -> ref
                | FunctionCall fc -> fc.AsString()

and ScalarExpression =
| Count of VectorExpression
with member this.AsString() = "notImpl ()"

and VectorExpression =
| Source of string
| Where of query:QueryExpression*operation:BooleanExpression // how do i define the body of the expression
with member this.AsString() = 
                match this with
                | Source sourceName -> sourceName
                | Where (query, operator) -> 
                    sprintf "%s | where %s" (query.AsString()) (operator.AsString())

and QueryExpression = 
| ScalarExpression of ScalarExpression
| VectorExpression of VectorExpression
with member this.AsString() = 
                match this with
                | ScalarExpression expr -> expr.AsString ()
                | VectorExpression expr -> expr.AsString ()

type QueryStatement =
| LetBinding
| QueryExpression of QueryExpression
with member this.AsString() = 
                match this with
                | LetBinding -> "let"
                | QueryExpression expr -> expr.AsString()


module Say =
    let wherequest = 
         VectorExpression.Where(
             VectorExpression (Source "requests"),
             {
                 left = PropertyReference "resultCode"
                 right = String "200"
                 operator = BooleanOperator.Equal
             } )
         |> VectorExpression
         |> QueryExpression
    


            

    let hello name = 
         printfn "%s" (wherequest.AsString()) 


