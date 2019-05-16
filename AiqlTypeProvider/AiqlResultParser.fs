namespace ExpressionBuilder

module ResultParer = 
    open Newtonsoft.Json
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open System
    open System.IO
    open System.Net.Http
    open Contract
    open FSharp.Control

    let serializer = JsonSerializer()

    type JsonTextReader with
        member __.AdvanceAsync() = 
            __.ReadAsync() |> Async.AwaitTask |> Async.Ignore

        /// match the provided JsoonTokenreader to the expectedtoken sequence
        /// the first token is matched without calling read on on the sequence
        /// the reader will only advance if there is at least two elements in the sequence
        member __.Expect expectedTokenSequence =
            let isMatch (tokenType, tokenValue) = (__.TokenType = tokenType) && (match tokenValue with | Some v -> __.Value.ToString().Equals(v, StringComparison.OrdinalIgnoreCase) | None -> true )
            let rec matchSeq e = 
                async {
                    match e with
                    | [h] when isMatch h -> return true
                    | h :: t when isMatch h -> 
                            do! __.AdvanceAsync()
                            return! matchSeq t    
                
                    | [] -> return true
                    | _ -> return false
                }
            matchSeq expectedTokenSequence

    let readRows<'T>(colDefs:ColumnDefinition[], reader:JsonTextReader) = asyncSeq {
        let typ = typeof<'T> 
        let propertyMap = 
            lazy
                if typ.IsClass then 
                    typ.GetRuntimeProperties()
                    |> Seq.map(fun prop -> prop.Name, prop)
                    |> Map.ofSeq
                else
                    failwith "not supported yet"

        match! reader.Expect [(JsonToken.PropertyName, Some ("Rows"));(JsonToken.StartArray, None)] with
        | true ->  
            let createInstance () = async {
                if FSharpType.IsRecord typ then
                    //let fields = FSharpType.GetRecordFields typ
                    let! parameterValues = 
                        asyncSeq {
                            while reader.TokenType <> JsonToken.EndArray do
                                yield reader.Value
                                do! reader.AdvanceAsync()
                        }
                        |> AsyncSeq.toArrayAsync
                    return FSharpValue.MakeRecord(typ, parameterValues) :?> 'T
                    // :> 

                    // note: we need to read all the field values in an array, in the same order
                    // as the fields in the FSharpType.GetRecordFields array
                    // possible optimisation  is to make sure that when the query returns a record
                    // the corresponding AIQL query should list the property in the expected order
                    
                    //failwith "record type not supported yet"
                elif typ.IsClass then
                    let lowestNumberOfConstructorParameter = 
                        typ.GetConstructors()
                        |> Seq.map (fun c -> c.GetParameters())
                        |> Seq.sortBy (fun p -> p.Length)
                        |> Seq.tryHead

                    match lowestNumberOfConstructorParameter with 
                    | Some [||] -> // the type has a parameter less constructor
                        let ob = System.Activator.CreateInstance<'T>()
                        let mutable count = 0
                        while reader.TokenType <> JsonToken.EndArray do
                            match propertyMap.Value.TryFind colDefs.[count].Name with
                            | Some prop when prop.CanWrite -> 
                                prop.SetValue(ob, Convert.ChangeType(reader.Value, prop.PropertyType))
                            | _ -> ()
                            count <- count+1
                            do! reader.AdvanceAsync()
                        do! reader.AdvanceAsync()
                        return ob
                    | Some _ -> // could be an anounymous type
                        let! parameterValues = 
                            asyncSeq {
                                while reader.TokenType <> JsonToken.EndArray do
                                    yield reader.Value
                                    do! reader.AdvanceAsync()
                            }
                            |> AsyncSeq.toArrayAsync
                        //let ob = System.Activator.CreateInstance(typ, parameterValues)
                        let ob = Activator.CreateInstance(typ, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.CreateInstance, null, parameterValues, null, null)
                        do! reader.AdvanceAsync()
                        return ob :?> 'T
                    | None -> 
                        return failwith (sprintf "class with no constructors are not supported '%O'" typ)
                else
                    return failwithf "type '%s' not supported yet" typ.Name
            }
                
            // skipping the start of the array
            do! reader.AdvanceAsync()

            let tryReadRow () = async {
                match! reader.Expect [(JsonToken.StartArray, None) ] with 
                | true ->
                    do! reader.AdvanceAsync()
                    let! instance = createInstance ()
                    return Some instance
                | false -> return None
            }
            
            yield! 
                AsyncSeq.initInfiniteAsync(fun _ -> tryReadRow ())
                |> AsyncSeq.takeWhile Option.isSome
                |> AsyncSeq.map Option.get

        | false -> 
            return failwith (sprintf "Unexpected format %O" reader.TokenType)
    }

    let readColumnMetadata<'T>(reader:JsonTextReader) = async {
        match! reader.Expect [ (JsonToken.PropertyName, Some("Columns") ) ; (JsonToken.StartArray, None ) ] with
        | true ->
            
            let colDefs = serializer.Deserialize<ColumnDefinition[]> reader

            if reader.TokenType = JsonToken.EndArray then 
                do! reader.AdvanceAsync()

            return colDefs

            //match reader with 
            //| JsonSequence [(JsonToken.PropertyName, Some ("Rows")); (JsonToken.StartArray, None)] _ ->
            
            //| r -> 
            //    failwith (sprintf "Unexpected format %O" r.TokenType)
        | false -> 
           return  failwith (sprintf "Unexpected format %O" reader.TokenType)
    }
        
    let readTableData<'T>(reader:JsonTextReader) = asyncSeq {
        match! reader.Expect [(JsonToken.PropertyName, Some( "Tables") ); (JsonToken.StartArray, None); (JsonToken.StartObject, None)] with
        | true -> 
            let! _ = reader.ReadAsync() |> Async.AwaitTask
            // skipping to the column defiition
            while ( not <| reader.Value.ToString().Equals("Columns", StringComparison.OrdinalIgnoreCase)) do
                do! reader.AdvanceAsync()
            let! columnDefs = readColumnMetadata<'T>(reader)
            yield! readRows<'T>(columnDefs, reader)

        | false -> 
            failwith (sprintf "Unexpected format %O" reader.TokenType)
    }

    let readResults<'T>(stream:Stream) = asyncSeq {
        use streamReader = new System.IO.StreamReader(stream)
        use jsonReader =  new JsonTextReader(streamReader)

        match! jsonReader.Expect [(JsonToken.None, None); (JsonToken.StartObject, None); (JsonToken.PropertyName, Some( "Tables") )] with
        | true ->
            yield! readTableData<'T>(jsonReader)
        | false -> 
            failwith (sprintf "Unexpected format %O" jsonReader.TokenType)
    }

    let createClient (address:string, apiKey:string) = 
        let client = new HttpClient(BaseAddress = Uri address)
        client.DefaultRequestHeaders.Add("x-api-key",sprintf "%s" apiKey)
        client

    let getSchema (address:string, apiKey:string) = async {
        use client = createClient (address, apiKey)
        let! result = client.GetAsync(sprintf "%s/query/schema" address) |> Async.AwaitTask
        if result.IsSuccessStatusCode then
            let! str = result.Content.ReadAsStringAsync() |> Async.AwaitTask
            let tableData = JsonConvert.DeserializeObject<TableDefnitions>(str)
            return tableData
        else
            let! resultText = result.Content.ReadAsStringAsync() |> Async.AwaitTask
            return failwith (sprintf "Failed query, Status code: %d;\n%s" (int result.StatusCode) resultText)
    }

    let sendRequest<'T> (address:string, apiKey:string) (request:string) = asyncSeq {
        use client = createClient (address, apiKey)
        let request = sprintf "%s/query?query=%s" address (Uri.EscapeDataString request)

        let! result = 
            client.GetAsync request
            |> Async.AwaitTask

        if result.IsSuccessStatusCode then
            let! resultStream = result.Content.ReadAsStreamAsync() |> Async.AwaitTask
            yield! readResults<'T>(resultStream)
        else
            let! resultText = result.Content.ReadAsStringAsync() |> Async.AwaitTask
            return failwith (sprintf "Failed query, Status code: %d;\n%s" (int result.StatusCode) resultText)
    }

            
