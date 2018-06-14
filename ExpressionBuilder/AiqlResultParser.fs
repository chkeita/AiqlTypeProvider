namespace ExpressionBuilder

module ResultParer = 
    open Newtonsoft.Json
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open System
    open System.IO
    open System.Net.Http
    open System.Web
    open Contact
    
    /// match the provided JsoonTokenreader to the expectedtoken sequence
    /// the first token is matched without calling read on on the sequence
    /// the reader will only advance if there is at least two elements in the sequence
    let (|JsonSequence|_|) expectedTokenSequence (reader:JsonTextReader) =
        let isMatch (tokenType, tokenValue) = (reader.TokenType = tokenType) && (match tokenValue with | Some v -> reader.Value.ToString().Equals(v, StringComparison.OrdinalIgnoreCase) | None -> true )
        let rec matchSeq  = 
            function
            | [h] when isMatch h -> Some () 
            | h :: t when isMatch h -> 
                    reader.Read() |> ignore
                    matchSeq t    
            
            | [] -> Some ()
            | _ -> None
        
        matchSeq expectedTokenSequence
    
    
    let readRows<'T>(colDefs:ColumnDefinition[], reader:JsonTextReader)= 
        let typ = typeof<'T> 
        match reader with 
        | JsonSequence [(JsonToken.StartArray, None); (JsonToken.StartArray, None)] _ ->  
            let createInstance () =
                if typ.IsClass then 
                    let ob = System.Activator.CreateInstance<'T>()
                    let mutable count = 0
                    while reader.TokenType <> JsonToken.EndArray do
                        //let prop = typ.GetProperty(colDefs.[count].ColumnName, BindingFlags.Public ||| BindingFlags.Instance)
                        let prop = typ.GetRuntimeProperty(colDefs.[count].Name)
                        if null <> prop && prop.CanWrite then
                            prop.SetValue(ob, Convert.ChangeType(reader.Value, prop.PropertyType))
                        count <- count+1
                        reader.Read() |> ignore
                    reader.Read() |> ignore
                    ob
                elif FSharpType.IsRecord typ then
                    failwith "record type not supported yet"
                else
                    failwithf "type '%s' not supported yet" typ.Name
                
    
            let tryReadRow () = 
                match reader with 
                | JsonSequence [(JsonToken.StartArray, None) ] _ ->
                    reader.Read() |> ignore
                    createInstance ()
                    |> Some
                | _ -> None
            
            Seq.initInfinite(fun _ -> tryReadRow ())
            |> Seq.takeWhile Option.isSome
            |> Seq.map Option.get
        | r -> 
                failwith (sprintf "Unexpected format %O" r.TokenType)

    let readColumnMetadata<'T>(reader:JsonTextReader) =
        match reader with
        | JsonSequence [ (JsonToken.PropertyName, Some("Columns") ) ; (JsonToken.StartArray, None ) ] _ ->
            let serializer = JsonSerializer()
            
            let colDefs = serializer.Deserialize<ColumnDefinition[]>(reader)
            match reader with 
            | JsonSequence [(JsonToken.EndArray, None); (JsonToken.PropertyName, Some ("Rows")); (JsonToken.StartArray, None)] _ ->
                readRows<'T>(colDefs, reader)
            | r -> 
                failwith (sprintf "Unexpected format %O" r.TokenType)
        | r -> 
            failwith (sprintf "Unexpected format %O" r.TokenType)
        
    let readTableData<'T>(reader:JsonTextReader) =
        match reader with
        | JsonSequence [(JsonToken.PropertyName, Some( "Tables") ); (JsonToken.StartArray, None); (JsonToken.StartObject, None)] _ -> 
            reader.Read() |> ignore
            // skipping to the column defiition
            while ( not <| reader.Value.ToString().Equals("Columns", StringComparison.OrdinalIgnoreCase)) do
                reader.Read() |> ignore
            let columnDefs = readColumnMetadata<'T>(reader)
            columnDefs 
        | r -> 
            failwith (sprintf "Unexpected format %O" r.TokenType)

    let readResults<'T>(stream:Stream) = 
        seq {
            use streamReader = new System.IO.StreamReader(stream)
            use jsonReader =  new JsonTextReader(streamReader)

            match jsonReader with
            | JsonSequence [(JsonToken.None, None); (JsonToken.StartObject, None); (JsonToken.PropertyName, Some( "Tables") )] _ ->
                for row in readTableData<'T>(jsonReader) do
                    yield row
            | tokenType -> 
                failwith (sprintf "Unexpected format %O" tokenType)
        }

    let createClient (address:string, apiKey:string) = 
        let client = new HttpClient(BaseAddress = Uri address)
        client.DefaultRequestHeaders.Add("x-api-key",sprintf "%s" apiKey)
        client

    let getSchema (address:string, apiKey:string) =
        async {
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

    let sendRequest<'T> (address:string, apiKey:string) (request:string) =
        async {
            use client = createClient (address, apiKey)
            let! result = 
                client.GetAsync(sprintf "%s/query?query=%s" address (HttpUtility.UrlEncode request))
                |> Async.AwaitTask

            if result.IsSuccessStatusCode then
                let! resultStream = result.Content.ReadAsStreamAsync() |> Async.AwaitTask
                return readResults<'T>(resultStream)
            else
                let! resultText = result.Content.ReadAsStringAsync() |> Async.AwaitTask
                return failwith (sprintf "Failed query, Status code: %d;\n%s" (int result.StatusCode) resultText)
        }

            
