namespace ExpressionBuilder

    
/// Translation of F# quotation to aiql
module ExpressionWriter =
    open Expression
    open System
    open Microsoft.FSharp.Reflection

    let getAiqlTypeString =
        function 
        | Bool -> "bool"
        | Datetime -> "datetime"
        | Dynamic -> "dynamic"
        | Guid -> "guid"
        | Int -> "int"
        | Long -> "long"
        | Double -> "double"
        | String -> "string"
        | Timespan -> "timespan"

    let getAiqlOperatorString =
        function
        | Plus -> "+"
        | Minus -> "-"
        | Divide -> "/" 
        | Multiply -> "*"
        | And -> "&&"
        | Or -> "||"
        | Equal -> "=="

    let getAiqlTabularOperator = 
        function
        | Where -> "where"
        | Take -> "take"
        | Top -> "top"
        | Limit -> "limit"
        | Sort -> "sort"
        | Project -> "project"


    let rec fromAiqlQuery q =
        q
        |> Seq.map fromAiqlStatement
        |> String.concat ";\n"

    and fromAiqlStatement =
        function
        | AiqlStatement.LetBinding (name, body) ->
            sprintf "let %s = %s" name (fromAiqlExpressionBody body)
       
        | AiqlStatement.AiqlExpression expr -> 
            fromAiqlExpression expr
            
    and fromAiqlExpression =
        function
        | AiqlExpression.TabularExpression (source, func, body) ->
            sprintf "%s | %O %s" (fromAiqlExpression source) func (fromAiqlExpressionBody body)
        | AiqlExpression.Table table ->
            table

    and fromAiqlExpressionBody =
        function
        | AiqlExpressionBody.ConstantExpression c -> c

        | AiqlExpressionBody.Var v -> v

        | AiqlExpressionBody.PropertyGet p -> p

        | AiqlExpressionBody.BinaryOperation (left, right, operator) ->
            sprintf "%s %s %s" (fromAiqlExpressionBody left) (getAiqlOperatorString operator) (fromAiqlExpressionBody right)
            
        | AiqlExpressionBody.AiqlExpression exp -> 
            fromAiqlExpression exp

        | AiqlExpressionBody.FunctionAppliation (name, args) ->
            sprintf "%s(%s)" name (args |> Seq.map fromAiqlExpressionBody |> String.concat ", " )

        | AiqlExpressionBody.Lambda (args, body) ->
            sprintf "(%s) { %s }"(args |> Seq.map (fun (name,typ) -> sprintf "%s:%s" name (getAiqlTypeString typ)) |> String.concat ", ") (fromAiqlExpressionBody body)
        
        | AiqlExpressionBody.PropPertyList props -> 
            props
            |> Seq.map(fun (name, body) -> sprintf "%s = %s" name (fromAiqlExpressionBody body) )
            |> String.concat ", "
