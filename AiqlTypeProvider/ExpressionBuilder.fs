module ExpressionBuilder
open System
open System.Reflection

type IAiqlQuery<'T> = 
    abstract Expression : Quotations.Expr<'T>

type SingleResultQuery<'T> = 
    inherit IAiqlQuery<'T>

let getTable (tableName:String) = new NotImplementedException () |> raise
let where  ([<ReflectedDefinition>] predicate ) query = new NotImplementedException () |> raise
let count (query:Quotations.Expr<'T[]>) : Quotations.Expr<'T> = new NotImplementedException () |> raise

let rec toAiql expr = 
    match expr with
    | Quotations.Patterns.Value (v,t) -> v.ToString()
    | Quotations.Patterns.PropertyGet (obj, propertyInfo, _) -> propertyInfo.Name
    | Quotations.Patterns.Var vr -> vr.Name
    | Quotations.DerivedPatterns.SpecificCall <@ getTable @> (_,_, [Quotations.Patterns.Value (v,t)]) ->
        v.ToString()
    | Quotations.DerivedPatterns.SpecificCall <@ where @> (_,_ ,whereArgs) -> 
        match whereArgs with
        | [Quotations.Patterns.Lambda ( arg,body);queryExpression ] ->
            match body with
            | Quotations.DerivedPatterns.SpecificCall <@ (=) @> (_,_,[left;right]) ->
                sprintf "%s | where (%s == %s)" (toAiql queryExpression) (toAiql left) (toAiql right)
            | _ -> "Nothing"
        | _ -> "Nothing"
    | _ -> "Nothing"

[<NoComparison>]
type QueryExpresion<'T> = {
    Expression: Quotations.Expr<'T>
}

type QueryExpresion = 
    static member Where ([<ReflectedDefinition>] predicate:Quotations.Expr<'a -> bool> )  =
                fun (this:QueryExpresion<'a[]>) -> { this with Expression =  <@ where %predicate %this.Expression @>}
        
    
    