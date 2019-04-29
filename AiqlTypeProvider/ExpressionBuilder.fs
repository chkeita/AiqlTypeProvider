namespace ExpressionBuilder

/// Translation of F# quotation to aiql
module Expression =
    open System
    open Microsoft.FSharp.Reflection

    type AiqlType =
    | Bool
    | Datetime
    | Dynamic
    | Guid
    | Int
    | Long
    | Double
    | String
    | Timespan
   
    type AiqlScalarOperator =
    | Plus
    | Minus
    | Divide
    | Multiply
    | And
    | Or
    | Equal

    type AiqlTabularOperator = 
    | Where
    | Take
    | Top
    | Limit
    | Sort
    | Project
    | Join
    

    and AiqlExpressionBody =
    | ConstantExpression of value:obj
    | BinaryOperation of left:AiqlExpressionBody*right:AiqlExpressionBody*operator:AiqlScalarOperator
    | AiqlExpression of AiqlExpression
    | PropertyGet of varName:string*propertyName:string
    | Var of string
    | Lambda of args:(string*AiqlType)[]*body:AiqlExpressionBody
    | FunctionAppliation of functionName:string*args:list<AiqlExpressionBody>
    | PropPertyList of list<string*AiqlExpressionBody>

    and AiqlExpression =
    | TabularExpression of source:list<AiqlExpression>*func:AiqlTabularOperator*body:AiqlExpressionBody
    //| JoinExpression of left:AiqlExpression*right:AiqlExpression*predicate:AiqlExpressionBody
    | Table of string
   
    type AiqlStatement = 
    | LetBinding of name:string*body:AiqlExpressionBody
    | AiqlExpression of AiqlExpression

    type AiqlQuery = list<AiqlStatement>
    type JoinedEntity<'Left,'Right> = {
        Left: 'Left
        Right: 'Right
    }

    let notSupported x = NotSupportedException(sprintf "%O" x) |> raise
    let getTable<'T> (tableName:String) : seq<'T> = NotImplementedException () |> raise
    let where<'elemType,'seqType when 'seqType :> seq<'elemType>>  ([<ReflectedDefinition>] predicate:'elemType -> bool ) (query:'seqType) : seq<'elemType> = new NotImplementedException () |> raise
    let count<'elemType,'seqType when 'seqType :> seq<'elemType>> (query:Quotations.Expr<'seqType>) : seq<'elemType> = NotImplementedException () |> raise
    let take<'elemType> (size: int) (query:seq<'elemType>) : seq<'elemType> = NotImplementedException () |> raise
    let top<'elemType> (size: int) (query:seq<'elemType>) : seq<'elemType> = NotImplementedException () |> raise
    let limit<'elemType> (size: int) (query:seq<'elemType>) : seq<'elemType> = NotImplementedException () |> raise
    let orderBy<'elemType,'result,'seqType when 'seqType :> seq<'elemType>>  ([<ReflectedDefinition>] predicate:'elemType -> 'result ) (query:'seqType) : seq<'elemType> = new NotImplementedException () |> raise
    let project<'elemType,'result,'seqType when 'seqType :> seq<'elemType>>  ([<ReflectedDefinition>] predicate:'elemType -> 'result ) (query:'seqType) : seq<'result> = new NotImplementedException () |> raise
    let join<'leftEntity,'leftSeq,'rightEntity,'rightSeq 
                when 'leftSeq :> seq<'leftEntity> 
                and 'rightSeq :> seq<'rightEntity>>  
                    ([<ReflectedDefinition>] predicate:'leftEntity*'rightEntity -> bool ) 
                    (left:'leftSeq, right:'rightSeq): JoinedEntity<'leftEntity,'rightEntity> = 
                
                    new NotImplementedException () |> raise

    let join2<'leftEntity,'leftSeq,'rightEntity,'rightSeq 
                when 'leftSeq :> seq<'leftEntity> 
                and 'rightSeq :> seq<'rightEntity>>  
                    ([<ReflectedDefinition>] predicate:JoinedEntity<'leftEntity,'rightEntity> -> bool ) 
                    (left:'leftSeq, right:'rightSeq): JoinedEntity<'leftEntity,'rightEntity> = 
    
                    new NotImplementedException () |> raise

    let (|TupledArgMethodCallPattern|_|) expr =
        let rec isMethodCall args expr =
            match expr with
            | Quotations.ExprShape.ShapeLambda (_, z) -> 
                isMethodCall args z
            | Quotations.Patterns.Call (_,methodInfo,_) ->
                Some (methodInfo,args)
            | Quotations.Patterns.Let (arg, body, remaining) ->
                isMethodCall (Map.add arg.Name body args) remaining
            | _ -> None
        isMethodCall Map.empty expr

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
                        //Quotations.DerivedPatterns.Lambdas (_, Quotations.Patterns.Call (_,methodInfo,_))
                        Quotations.DerivedPatterns.Lambdas (_, TupledArgMethodCallPattern (methodInfo,_))
                    ])
                ]) ->
            match left with 
            | Quotations.Patterns.NewTuple [a; b]-> 
                // if left is a tuple the last two parameter of the the method will match th tuple
                
                (Quotations.Expr.Call (methodInfo, [firstArg; a; b] )) |> Some
            | _ ->
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

    let (|MatchOpErator|_|) expr =
        let operators =
            [
                <@@ (=) @@>, AiqlScalarOperator.Equal
                <@@ (+) @@>, AiqlScalarOperator.Plus
                <@@ (-) @@>, AiqlScalarOperator.Minus
                <@@ (&&) @@>, AiqlScalarOperator.And
                <@@ (||) @@>, AiqlScalarOperator.Or
                <@@ (*) @@>, AiqlScalarOperator.Multiply
            ]

        operators
        |> Seq.tryPick(
            fun (fsharpOperator, aiqlOperator) ->
                match expr with 
                | Quotations.DerivedPatterns.SpecificCall fsharpOperator (_,_,[left;right]) -> Some (aiqlOperator, left, right)
                | _ -> None)

    let dotNetTypeToAiqlMapping =
        [
            "System.Boolean", AiqlType.Bool
            "System.DateTime", AiqlType.Datetime
            "System.Object", AiqlType.Dynamic
            "System.Guid", AiqlType.Guid
            "System.Int32", AiqlType.Int
            "System.Int64", AiqlType.Long
            "System.Double", AiqlType.Double
            "System.String", AiqlType.String
            "System.TimeSpan", AiqlType.Timespan
        ] |> Map.ofSeq

    let getAiqlType (ty:Type) =
        match dotNetTypeToAiqlMapping.TryFind ty.FullName with
        | Some t -> t
        | _ -> failwith "unknown type"

    /// Converts a query expression to an aiql query
    let rec toAiqlQuery = 
        function
        | Quotations.Patterns.Let (arg, body, remaining) ->
            AiqlStatement.LetBinding(arg.Name, toBodyExpr None body) :: (toAiqlQuery remaining)
            
        | exp -> [toAiqlExpression exp |> AiqlStatement.AiqlExpression]

    and toTabularExpression operator = 
        function
        | [Quotations.Patterns.Lambda (arg,body); queryExpression ] ->
            AiqlExpression.TabularExpression  ( [toAiqlExpression queryExpression] , operator, toBodyExpr (Some ([arg], operator)) body)
        | exp -> notSupported exp

    /// Converts a query expression to an aiql query
    and toAiqlExpression =
        function
        | PipePattern expr -> 
            toAiqlExpression expr

        | Quotations.DerivedPatterns.SpecificCall <@ getTable @> (_,_, [Quotations.Patterns.Value (v,t)]) ->
            v.ToString() |> AiqlExpression.Table

        | Quotations.DerivedPatterns.SpecificCall <@ where @> (_,_ ,whereArgs) ->
            toTabularExpression AiqlTabularOperator.Where whereArgs

        | Quotations.DerivedPatterns.SpecificCall <@ take @> (_,_ ,takeArgs) ->
            match takeArgs with
            | [Quotations.Patterns.Value (v,t); queryExpression ] ->
                AiqlExpression.TabularExpression  ([toAiqlExpression queryExpression], AiqlTabularOperator.Take, (AiqlExpressionBody.ConstantExpression v ))
            | exp -> notSupported exp

        | Quotations.DerivedPatterns.SpecificCall <@ top @> (_,_ ,topArgs) ->
            match topArgs with
            | [Quotations.Patterns.Value (v,t); queryExpression ] ->
                AiqlExpression.TabularExpression  ([toAiqlExpression queryExpression], AiqlTabularOperator.Top, (AiqlExpressionBody.ConstantExpression v ))
            | exp -> notSupported exp

        | Quotations.DerivedPatterns.SpecificCall <@ limit @> (_,_ ,limitArgs) ->
            match limitArgs with
            | [Quotations.Patterns.Value (v,t); queryExpression ] ->
                AiqlExpression.TabularExpression  ([toAiqlExpression queryExpression], AiqlTabularOperator.Limit, (AiqlExpressionBody.ConstantExpression v))
                
            | exp -> notSupported exp
        | Quotations.DerivedPatterns.SpecificCall <@ orderBy @> (_,_ ,orderByArgs)  ->
            toTabularExpression AiqlTabularOperator.Sort orderByArgs

        | Quotations.DerivedPatterns.SpecificCall <@ project @> (_,_ ,projectArgs) ->
            toTabularExpression AiqlTabularOperator.Project projectArgs

        | Quotations.DerivedPatterns.SpecificCall <@ join @> (_,_ ,joinArgs) ->
            match joinArgs with
            | [Quotations.DerivedPatterns.Lambdas ([vars], body ); queryLeft; queryRight ] ->
                AiqlExpression.TabularExpression  ( [toAiqlExpression queryLeft; toAiqlExpression queryRight] , AiqlTabularOperator.Join , toBodyExpr (Some (vars ,AiqlTabularOperator.Join)) body)
                
            | exp -> notSupported exp
            
        | Quotations.Patterns.PropertyGet (obj, propertyInfo, _) -> 
            AiqlExpression.Table propertyInfo.Name
        
        | exp -> notSupported exp

    and toBodyExpr operator =
        function
        | Quotations.Patterns.Value (v,t) ->
            AiqlExpressionBody.ConstantExpression v

        | Quotations.Patterns.Var vr -> 
            AiqlExpressionBody.Var vr.Name

        | Quotations.Patterns.Coerce  (exp,returnType) ->
            toBodyExpr operator exp

        | MatchOpErator (aiqlOperator, left, right) ->
            AiqlExpressionBody.BinaryOperation(toBodyExpr operator left, toBodyExpr operator right, aiqlOperator)

        | Quotations.DerivedPatterns.Lambdas ([vars], bd) ->
            let vars = vars |> Seq.map (fun x -> x.Name, (getAiqlType x.Type)) |> Seq.toArray
            AiqlExpressionBody.Lambda(vars, toBodyExpr operator bd)

        | RecorCreationPattern (typ, recordArgs, exprs) ->
            let fields = FSharpType.GetRecordFields typ
            Seq.zip fields exprs
            |> Seq.map (
                function
                | (f,Quotations.Patterns.Var vr) ->
                    let value = 
                        recordArgs.TryFind vr.Name
                        |> Option.defaultValue (Quotations.Expr.Var vr)
                    
                    f.Name, (toBodyExpr operator value)
                | (f,exp) ->
                    f.Name, (toBodyExpr operator exp)
                )
            |> Seq.toList
            |> AiqlExpressionBody.PropPertyList

        | Quotations.DerivedPatterns.Applications (Quotations.Patterns.Var v, [tupleArgs]) ->
            AiqlExpressionBody.FunctionAppliation(v.Name, tupleArgs |> List.map (toBodyExpr operator) )

        | Quotations.Patterns.PropertyGet (Some (Quotations.Patterns.Var varName), propertyInfo, _) -> 
            match operator with
            | Some ([left; right], AiqlTabularOperator.Join) -> 
                let propertyName = if varName.Name = left.Name then "$left" else "$right"

                AiqlExpressionBody.PropertyGet (propertyName, propertyInfo.Name)
            | _ -> 
                AiqlExpressionBody.PropertyGet (varName.Name, propertyInfo.Name)

        | exp -> notSupported exp
