namespace AiqlContract

module ResultParer = 
    open Newtonsoft.Json
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open System
    open System.IO

    
    /// match the provided JsoonTokenreader to the expectedtoken sequence
    /// the first token is matched without calling read on on the sequence
    /// the reader will only advance if there is at least two elements in the sequence
    let (|JsonSequence|_|) expectedTokenSequence (reader:JsonTextReader) =
        let isMatch (tokenType, tokenValue) = (reader.TokenType = tokenType) && (match tokenValue with | Some v -> reader.Value = v | None -> true )
        let rec matchSeq  = 
            function
            | [h] when isMatch h -> Some () 
            | h :: t when isMatch h -> 
                    reader.Read() |> ignore
                    matchSeq t    
            
            | [] -> Some ()
            | _ -> None
        
        matchSeq expectedTokenSequence
    
    
    let readRows(typ:Type, colDefs:ColumnDefinition[], reader:JsonTextReader)= 
        match reader with 
        | JsonSequence [(JsonToken.StartArray, None); (JsonToken.StartArray, None)] _ ->  
            let length = colDefs |> Array.length
            let serializer = JsonSerializer()
            let createInstance () =
                if typ.IsClass then 
                    let ob = System.Activator.CreateInstance(typ)
                    let mutable count = 0
                    while reader.TokenType <> JsonToken.EndArray do
                        //let prop = typ.GetProperty(colDefs.[count].ColumnName, BindingFlags.Public ||| BindingFlags.Instance)
                        let prop = typ.GetRuntimeProperty(colDefs.[count].ColumnName)
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

    let readColumnMetadata(typ:Type, reader:JsonTextReader) =
        match reader with
        | JsonSequence [ (JsonToken.PropertyName, Some( box "Columns") ) ; (JsonToken.StartArray, None ) ] _ ->
            let serializer = JsonSerializer()
            let colDefs = serializer.Deserialize<ColumnDefinition[]>(reader)
            match reader with 
            | JsonSequence [(JsonToken.EndArray, None); (JsonToken.PropertyName, Some (box "Rows")); (JsonToken.StartArray, None)] _ ->
                readRows(typ, colDefs, reader)
            | r -> 
                failwith (sprintf "Unexpected format %O" r.TokenType)
        | r -> 
            failwith (sprintf "Unexpected format %O" r.TokenType)
        
    let readTableData(typ:Type, reader:JsonTextReader) =
        match reader with
        | JsonSequence [(JsonToken.PropertyName, Some( box "Tables") ); (JsonToken.StartArray, None); (JsonToken.StartObject, None)] _ -> 
            reader.Read() |> ignore
            // skipping to the column defiition
            while ( reader.Value.ToString() <> "Columns") do
                reader.Read() |> ignore
            let columnDefs = readColumnMetadata(typ, reader)
            columnDefs 
        | r -> 
            failwith (sprintf "Unexpected format %O" r.TokenType)

    let readResults(typ:Type, stream:Stream) = 
        seq {
            use streamReader = new System.IO.StreamReader(stream)
            use jsonReader =  new JsonTextReader(streamReader)

            match jsonReader with
            | JsonSequence [(JsonToken.None, None); (JsonToken.StartObject, None); (JsonToken.PropertyName, Some( box "Tables") )] _ ->
                for row in readTableData(typ, jsonReader) do
                    yield row
            | tokenType -> 
                failwith (sprintf "Unexpected format %O" tokenType)
        }

    let readResultsTyped<'T> (stream:Stream) : seq<'T> = 
        seq {
            for row in readResults(typeof<'T>, stream) do
                yield row :?> 'T
        }
            
