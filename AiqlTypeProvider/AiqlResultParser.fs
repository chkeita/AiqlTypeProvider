namespace ExpressionBuilder

module ResultParer = 
    open Newtonsoft.Json
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open System
    open System.IO
    open System.Net.Http
    open Contract

    let serializer = JsonSerializer()
    
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
        let propertyMap = 
            lazy
                if typ.IsClass then 
                    typ.GetRuntimeProperties()
                    |> Seq.map(fun prop -> prop.Name, prop)
                    |> Map.ofSeq
                else
                    failwith "not supported yet"

        match reader with
        | JsonSequence [(JsonToken.PropertyName, Some ("Rows"));(JsonToken.StartArray, None)] _ ->  
            let createInstance () =
                if typ.IsClass then
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
                            reader.Read() |> ignore
                        reader.Read() |> ignore
                        ob
                    | Some _ -> // could be an anounymous type
                        let parameterValues = 
                            seq {
                                while reader.TokenType <> JsonToken.EndArray do
                                    yield reader.Value
                                    reader.Read() |> ignore
                            }
                            |> Seq.toArray
                        let ob = System.Activator.CreateInstance(typ, parameterValues)
                        reader.Read() |> ignore
                        ob :?> 'T
                    | None -> 
                        failwith (sprintf "class with no constructors are not supported '%O'" typ)
                        
                elif FSharpType.IsRecord typ then
                    //let fields = FSharpType.GetRecordFields typ
                    FSharpValue.MakeRecord(typ, serializer.Deserialize<obj[]> reader ) :?> 'T
                    // :> 

                    // note: we need to read all the field values in an array, in the same order
                    // as the fields in the FSharpType.GetRecordFields array
                    // possible optimisation  is to make sure that when the query returns a record
                    // the corresponding AIQL query should list the property in the expected order
                    
                    //failwith "record type not supported yet"
                else
                    failwithf "type '%s' not supported yet" typ.Name
                
            // skipping the start of the array
            reader.Read() |> ignore

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
            
            let colDefs = serializer.Deserialize<ColumnDefinition[]> reader

            if reader.TokenType = JsonToken.EndArray then 
                reader.Read() |> ignore

            colDefs

            //match reader with 
            //| JsonSequence [(JsonToken.PropertyName, Some ("Rows")); (JsonToken.StartArray, None)] _ ->
            
            //| r -> 
            //    failwith (sprintf "Unexpected format %O" r.TokenType)
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
            readRows<'T>(columnDefs, reader)

             
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
            let request = sprintf "%s/query?query=%s" address (Uri.EscapeDataString request)

            let! result = 
                client.GetAsync request
                |> Async.AwaitTask

            if result.IsSuccessStatusCode then
                let! resultStream = result.Content.ReadAsStreamAsync() |> Async.AwaitTask
                return readResults<'T>(resultStream)
            else
                let! resultText = result.Content.ReadAsStringAsync() |> Async.AwaitTask
                return failwith (sprintf "Failed query, Status code: %d;\n%s" (int result.StatusCode) resultText)
        }

            
