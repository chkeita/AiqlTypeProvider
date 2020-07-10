namespace ExpressionBuilder
    
/// Translation of F# quotation to aiql
module ExpressionWriter =
    open Expression

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
        | Join -> "join"
        | Extend -> "extend"

    let rec fromAiqlQuery q =
        q
        |> Seq.map fromAiqlStatement
        |> String.concat ";\n"

    and fromAiqlStatement =
        function
        | AiqlStatement.LetBinding (name, body) as outer ->
            sprintf "let %s = %s" name (fromAiqlExpressionBody None  body)
       
        | AiqlStatement.AiqlExpression expr -> 
            fromAiqlExpression expr
            
    and fromAiqlExpression =
        function
        | AiqlExpression.TabularExpression ([left; right], AiqlTabularOperator.Join, predicate) as outer ->
            sprintf "%s | join (%s) ON  %s" (fromAiqlExpression left) (fromAiqlExpression right) (fromAiqlExpressionBody (Some outer) predicate)
        | AiqlExpression.TabularExpression ([source], func, body) as outer ->
            sprintf "%s | %O %s" (fromAiqlExpression source) (getAiqlTabularOperator func) (fromAiqlExpressionBody (Some outer) body)
        | AiqlExpression.Table table ->
            table
        | exp -> notSupported (sprintf "unsupported expression %A" exp)

    and fromAiqlExpressionBody outerExpression =
        function
        | AiqlExpressionBody.ConstantExpression v ->
            match v with
            | :? string -> sprintf "\"%O\""  v
            | _ -> sprintf "%O" v

        | AiqlExpressionBody.Var v -> v

        | AiqlExpressionBody.PropertyGet (name, p) -> 
            match outerExpression with
            | Some (AiqlExpression.TabularExpression (_, AiqlTabularOperator.Join, _) ) -> 
                sprintf "%s.%s" name p
            | _ -> p

        | AiqlExpressionBody.BinaryOperation (left, right, operator) ->
            sprintf "%s %s %s" (fromAiqlExpressionBody outerExpression left) (getAiqlOperatorString operator) (fromAiqlExpressionBody outerExpression right)
            
        | AiqlExpressionBody.AiqlExpression exp -> 
            fromAiqlExpression exp

        | AiqlExpressionBody.FunctionAppliation (name, args) ->
            sprintf "%s(%s)" name (args |> Seq.map (fromAiqlExpressionBody outerExpression) |> String.concat ", " )

        | AiqlExpressionBody.Lambda (args, body) ->
            sprintf "(%s) { %s }"(args |> Seq.map (fun (name,typ) -> sprintf "%s:%s" name (getAiqlTypeString typ)) |> String.concat ", ") (fromAiqlExpressionBody outerExpression body)
        
        | AiqlExpressionBody.PropertyList props -> 
            props
            |> Seq.map(fun (name, body) -> sprintf "%s = %s" name (fromAiqlExpressionBody outerExpression body) )
            |> String.concat ", "
