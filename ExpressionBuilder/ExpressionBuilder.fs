namespace ExpressionBuilder

/// Translation of F# quotation to aiql
module Expression =
    open System
    open Microsoft.FSharp.Reflection
    open System.Linq.Expressions
    open System.Linq.Expressions

    let notSupported x = new NotSupportedException(sprintf "%O" x) |> raise

    let getTable<'T> (tableName:String) : 'T = new NotImplementedException () |> raise
    let where<'elemType,'seqType when 'seqType :> seq<'elemType>>  ([<ReflectedDefinition>] predicate:'elemType -> bool ) (query:'seqType) : seq<'elemType> = new NotImplementedException () |> raise
    let count<'elemType,'seqType when 'seqType :> seq<'elemType>> (query:Quotations.Expr<'seqType>) : seq<'elemType> = new NotImplementedException () |> raise
    let take<'elemType> (size: int) (query:seq<'elemType>) : seq<'elemType> = new NotImplementedException () |> raise
    let top<'elemType> (size: int) (query:seq<'elemType>) : seq<'elemType> = new NotImplementedException () |> raise
    let limit<'elemType> (size: int) (query:seq<'elemType>) : seq<'elemType> = new NotImplementedException () |> raise
    let orderBy<'elemType,'result,'seqType when 'seqType :> seq<'elemType>>  ([<ReflectedDefinition>] predicate:'elemType -> 'result ) (query:'seqType) : seq<'elemType> = new NotImplementedException () |> raise
    let project<'elemType,'result,'seqType when 'seqType :> seq<'elemType>>  ([<ReflectedDefinition>] predicate:'elemType -> 'result ) (query:'seqType) : seq<'result> = new NotImplementedException () |> raise

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

    /// Handling of Record creation
    /// When the members of a record are initialized an order which is diffretn
    /// from the order in the record type defnition. The quoted expression will be iclude
    /// intialisation statements
    /// Ex:
    /// type Test = { T1:string; T2:string; T3:string}
    /// <@ {T3 = "3"; T2 = "2"; T1 = "1" } @>
    /// // Let (T3, Value ("3"), Let (T2, Value ("2"), NewRecord (Test, Value ("1"), T2, T3)))

    let (|RecorCreationPattern|_|) expr =
        let rec isRecordCreation args expr =
            match expr with
            | Quotations.Patterns.NewRecord (typ, exprs) ->
                Some (typ, args, exprs)
            | Quotations.Patterns.Let (arg, body, remaining) ->
                isRecordCreation (Map.add arg.Name body args) remaining
            | _ -> None
        isRecordCreation Map.empty expr

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
        | PipePattern expr -> toAiql expr
        
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
                sprintf "%s | where %s" (toAiql queryExpression) (toAiql body)
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
                sprintf "%s | sort by %s" (toAiql queryExpression) (toAiql body)
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
            sprintf "(%s) { %s }"(vars |> Seq.map (fun x -> sprintf "%s:%s"x.Name (getAiqlType x.Type)) |> String.concat ", ") (toAiql bd)
        | RecorCreationPattern (typ, args, exprs) ->
            let fields = FSharpType.GetRecordFields typ
            Seq.zip fields exprs
            |> Seq.map (
                function
                | (f,Quotations.Patterns.Var vr) ->
                    let value = 
                        args.TryFind vr.Name
                        |> Option.defaultValue (Quotations.Expr.Var vr)
                    sprintf "%s = %s"f.Name (toAiql value)
                | (f,exp) -> sprintf "%s = %s"f.Name (toAiql exp)
                )
            |> String.concat ", "
        | Quotations.Patterns.Let (arg, body, remaining) ->
            sprintf "let %s = %s;\n%s" arg.Name (toAiql body) (toAiql remaining)
        | Quotations.DerivedPatterns.Applications (Quotations.Patterns.Var v, [tupleArgs]) ->
            sprintf "%s(%s)" v.Name (tupleArgs |> Seq.map toAiql |> String.concat ", " )
        | exp -> notSupported exp