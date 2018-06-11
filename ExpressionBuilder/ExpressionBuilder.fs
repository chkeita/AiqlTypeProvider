namespace ExpressionBuilder

module Expression = 
    open System
    open System.Reflection

    let notSupported x = new NotSupportedException(sprintf "%O" x) |> raise

    let getTable<'T> (tableName:String) : 'T = new NotImplementedException () |> raise
    let where<'elemType,'seqType when 'seqType :> seq<'elemType>>  ([<ReflectedDefinition>] predicate:'elemType -> bool ) (query:'seqType) : Quotations.Expr<'seqType> = new NotImplementedException () |> raise
    let count<'elemType,'seqType when 'seqType :> seq<'elemType>> (query:Quotations.Expr<'seqType>) : Quotations.Expr<'seqType> = new NotImplementedException () |> raise
    let take<'elemType> (size: int) (query:seq<'elemType>) : seq<'elemType> = new NotImplementedException () |> raise


    let (|PipePattern|_|) = function
        | Quotations.DerivedPatterns.SpecificCall 
            <@ (|>) @> 
            (_, _, [
                left
                Quotations.ExprShape.ShapeCombination (
                    _,
                    [
                        firstArg
                        Quotations.DerivedPatterns.Lambdas (_, Quotations.Patterns.Call (_,methodInfo,_))
                    ])
                ]) ->
            (Quotations.Expr.Call (methodInfo, [firstArg; left] )) |> Some
        | _ -> None

    let dotNetTypeToAiqlMapping = 
        [
            "System.Boolean", "bool"
            "System.DateTime", "datetime"
            "System.Object", "dynamic"
            "System.Guid", "guid"
            "System.Int32", "int"
            "System.Int64", "long"
            "System.Double", "double"
            "System.String", "string"
            "System.TimeSpan", "timespan"
        ] |> Map.ofSeq

    let getAiqlType (ty:Type) =
        match dotNetTypeToAiqlMapping.TryFind ty.FullName with
        | Some t -> t
        | _ -> "unknown type"

    let rec toAiql = 
        function
        | Quotations.Patterns.Lambda(parameters, body) -> 
            toAiql body
        | PipePattern expr -> toAiql expr
        | Quotations.Patterns.Let (arg, body, remaining) ->
            match body with
            | Quotations.Patterns.Value (v,t) -> 
                sprintf "let %s = %O;\n%s" arg.Name (v) (toAiql remaining)
            | Quotations.DerivedPatterns.Lambdas ([vars], bd) ->
                sprintf "let %s = (%s) { %s };\n%s" arg.Name (vars |> Seq.map (fun x -> sprintf "%s: %s"x.Name (getAiqlType x.Type)) |> String.concat ",") (toAiql bd) (toAiql remaining)
            | exp -> notSupported exp
        | Quotations.Patterns.Value (v,t) -> v.ToString()
        | Quotations.Patterns.PropertyGet (obj, propertyInfo, _) -> propertyInfo.Name
        | Quotations.Patterns.Var vr -> vr.Name
        | Quotations.DerivedPatterns.SpecificCall <@ getTable @> (_,_, [Quotations.Patterns.Value (v,t)]) -> v.ToString()
        | Quotations.DerivedPatterns.SpecificCall <@ where @> (_,_ ,whereArgs) -> 
            match whereArgs with
            | [Quotations.Patterns.Lambda (arg,body);queryExpression ] ->
                match body with
                | Quotations.DerivedPatterns.SpecificCall <@ (=) @> (_,_,[left;right]) ->
                    sprintf "%s | where (%s == %s)" (toAiql queryExpression) (toAiql left) (toAiql right)
                | Quotations.DerivedPatterns.SpecificCall <@ (+) @> (_,_,[left;right]) ->
                    sprintf "%s | where (%s + %s)" (toAiql queryExpression) (toAiql left) (toAiql right)
                | Quotations.DerivedPatterns.SpecificCall <@ (/) @> (_,_,[left;right]) ->
                    sprintf "%s | where (%s / %s)" (toAiql queryExpression) (toAiql left) (toAiql right)
                | exp -> notSupported exp
            | exp -> notSupported exp
        | Quotations.DerivedPatterns.SpecificCall <@ take @> (_,_ ,limitArgs) ->
            match limitArgs with
            | [Quotations.Patterns.Value (v,t); queryExpression ] ->
                sprintf "%s | limit %O" (toAiql queryExpression) v
            | exp -> notSupported exp
            

        | Quotations.Patterns.Coerce  (exp,returnType) -> 
            toAiql exp
        | exp -> notSupported exp



        
    
    