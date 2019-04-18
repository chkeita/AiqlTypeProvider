namespace ExpressionBuilder

/// Contains the types used to deserialized the result of an appinsight query 
module Contact =
    open System.Runtime.Serialization

    /// Contains the defnition of the columns from a table in appinsight
    [<DataContract>]
    type ColumnDefinition = {
        [<DataMember(Name="Name")>]
        _Name: string
        [<DataMember(Name="Type")>]
        _Type: string
        [<DataMember(Name="ColumnName")>]
        _ColumnName: string
        [<DataMember(Name="DataType")>]
        _DataType: string
        [<DataMember>]
        ColumnType: string
    }
    with 
        member x.Name =
            match (Option.ofObj x._Name), (Option.ofObj x._ColumnName) with
            | None, None -> failwithf "No name provided"
            | Some n, None -> n
            | _, Some n -> n

        member x.DataType =
            match (Option.ofObj x._Type), (Option.ofObj x._DataType) with
            | None, None -> failwithf "No data type provided"
            | Some n, None -> n
            | _, Some n -> n

    /// Defnition of a table in App insight
    type TableDefnition = {
        TableName: string
        Columns: ColumnDefinition[]
        Rows: string[][]
    }

    /// List of the table difinitions in app insight
    type TableDefnitions = {
       Tables : TableDefnition[] 
    }

    /// Definitions of a types returned by a query
    type ResultTableDefinition = {
        Name: string
        Columns: ColumnDefinition[]
    }

    /// List of the definitions of the types returned by a query
    type ResultDefinitions = {
        Tables : ResultTableDefinition[] 
    }

    type ErrorMessage = {
        Message: string
        code: string
    }

    type ErrorResult = {
        Error : ErrorMessage
    } 
