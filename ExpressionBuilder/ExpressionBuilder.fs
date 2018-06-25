namespace ExpressionBuilder

/// Translation of F# quotation to aiql 
module Expression =
    open System
    open Microsoft.FSharp.Reflection

    let notSupported x = new NotSupportedException(sprintf "%O" x) |> raise
    
    let getTable<'T> (tableName:String) : 'T = new NotImplementedException () |> raise
    let where<'elemType,'seqType when 'seqType :> seq<'elemType>>  ([<ReflectedDefinition>] predicate:'elemType -> bool ) (query:'seqType) : seq<'elemType> = new NotImplementedException () |> raise
    let count<'elemType,'seqType when 'seqType :> seq<'elemType>> (query:Quotations.Expr<'seqType>) : seq<'elemType> = new NotImplementedException () |> raise
    let take<'elemType> (size: int) (query:seq<'elemType>) : seq<'elemType> = new NotImplementedException () |> raise
    let top<'elemType> (size: int) (query:seq<'elemType>) : seq<'elemType> = new NotImplementedException () |> raise
    let limit<'elemType> (size: int) (query:seq<'elemType>) : seq<'elemType> = new NotImplementedException () |> raise
    let orderBy<'elemType,'result,'seqType when 'seqType :> seq<'elemType>>  ([<ReflectedDefinition>] predicate:'elemType -> 'result ) (query:'seqType) : seq<'elemType> = new NotImplementedException () |> raise
    let project<'elemType,'result,'seqType when 'seqType :> seq<'elemType>>  ([<ReflectedDefinition>] predicate:'elemType -> 'result ) (query:'seqType) : seq<'result> = new NotImplementedException () |> raise
    //let distinct
    //let find
    //let getschema


    //module aiqlStringFunctions =
    //startswith, 
    //!startswith 
    //has*, 
    //!has 
    //contains, 
    //!contains, 
    //containscs 
    //hasprefix, 
    //!hasprefix, 
    //hassuffix, 
    //!hassuffix 
    //in, 
    //!in 
    //matches regex 
        
    /// handling of (|>) in an expression
    /// We convert the pipe expression to a method call (a |> b  =>  b a)
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

    /// Converts a query expression to an aiql query
    let rec toAiql = 
        function
        | Quotations.Patterns.Lambda(parameters, body) -> 
            toAiql body
        | PipePattern expr -> toAiql expr
        | Quotations.Patterns.Let (arg, body, remaining) ->
            sprintf "let %s = %s;\n%s" arg.Name (toAiql body) (toAiql remaining)
        | Quotations.Patterns.Value (v,t) ->
            match v with 
            | :? string -> sprintf "\"%O\""  v
            | _ -> sprintf "%O" v
        | Quotations.Patterns.PropertyGet (obj, propertyInfo, _) -> propertyInfo.Name
        | Quotations.Patterns.Var vr -> vr.Name
        | Quotations.DerivedPatterns.SpecificCall <@ getTable @> (_,_, [Quotations.Patterns.Value (v,t)]) -> v.ToString()
        | Quotations.DerivedPatterns.SpecificCall <@ where @> (_,_ ,whereArgs) -> 
            match whereArgs with
            | [Quotations.Patterns.Lambda (arg,body);queryExpression ] ->
                sprintf "%s | where (%s)" (toAiql queryExpression) (toAiql body)
            | exp -> notSupported exp
        | Quotations.DerivedPatterns.SpecificCall <@ take @> (_,_ ,takeArgs) ->
            match takeArgs with
            | [Quotations.Patterns.Value (v,t); queryExpression ] ->
                sprintf "%s | take %O" (toAiql queryExpression) v
            | exp -> notSupported exp

        | Quotations.DerivedPatterns.SpecificCall <@ top @> (_,_ ,topArgs) ->
            match topArgs with
            | [Quotations.Patterns.Value (v,t); queryExpression ] ->
                sprintf "%s | top %O" (toAiql queryExpression) v
            | exp -> notSupported exp

        | Quotations.DerivedPatterns.SpecificCall <@ limit @> (_,_ ,limitArgs) ->
            match limitArgs with
            | [Quotations.Patterns.Value (v,t); queryExpression ] ->
                sprintf "%s | limit %O" (toAiql queryExpression) v
            | exp -> notSupported exp
        | Quotations.DerivedPatterns.SpecificCall <@ orderBy @> (_,_ ,orderByArgs) -> 
            match orderByArgs with
            | [Quotations.Patterns.Lambda (arg,body);queryExpression ] ->
                sprintf "%s | sort by (%s)" (toAiql queryExpression) (toAiql body)
            | exp -> notSupported exp
        | Quotations.DerivedPatterns.SpecificCall <@ project @> (_,_ ,projectArgs) -> 
            match projectArgs with
            | [Quotations.Patterns.Lambda (arg,body);queryExpression ] ->
                sprintf "%s | project %s" (toAiql queryExpression) (toAiql body)
            | exp -> notSupported exp
        | Quotations.Patterns.Coerce  (exp,returnType) -> 
            toAiql exp
        | Quotations.DerivedPatterns.SpecificCall <@ (=) @> (_,_,[left;right]) ->
            sprintf "%s == %s" (toAiql left) (toAiql right)
        | Quotations.DerivedPatterns.SpecificCall <@ (+) @> (_,_,[left;right]) ->
            sprintf "%s + %s" (toAiql left) (toAiql right)
        | Quotations.DerivedPatterns.SpecificCall <@ (/) @> (_,_,[left;right]) ->
            sprintf "%s / %s" (toAiql left) (toAiql right)
        | Quotations.DerivedPatterns.SpecificCall <@ (*) @> (_,_,[left;right]) ->
            sprintf "%s * %s" (toAiql left) (toAiql right)
        | Quotations.DerivedPatterns.SpecificCall <@ (&&) @> (_,_,[left;right]) ->
            sprintf "%s and %s" (toAiql left) (toAiql right)
        | Quotations.DerivedPatterns.SpecificCall <@ (||) @> (_,_,[left;right]) ->
            sprintf "%s or %s" (toAiql left) (toAiql right)
        | Quotations.Patterns.Value (v,t) -> 
            sprintf "%O" v
        | Quotations.DerivedPatterns.Lambdas ([vars], bd) ->
            sprintf "(%s) { %s }"(vars |> Seq.map (fun x -> sprintf "%s: %s"x.Name (getAiqlType x.Type)) |> String.concat ",") (toAiql bd)
        | Quotations.Patterns.NewRecord (typ, exprs) ->
            let fields = FSharpType.GetRecordFields typ
            Seq.zip fields exprs
            |> Seq.map (fun (f,exp) -> sprintf "%s = (%s)"f.Name (toAiql exp))
            |> String.concat ","

        | exp -> notSupported exp