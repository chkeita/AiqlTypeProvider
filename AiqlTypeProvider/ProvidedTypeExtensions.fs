namespace ProviderImplementation.ProvidedTypes
open System
open System.Reflection

/// We use this type to retrieve the custom attributes of a record type
type MyRecordType = { Dummy1:string ; Dummy2: int}

type ProvidedRecordDefnition (className:string, baseType, recordFields :seq<string*Type>, ?hideObjectMethods, ?nonNullable, ?isErased, ?isSealed, ?isInterface)  as this =
    inherit ProvidedTypeDefinition(className, baseType, ?hideObjectMethods = hideObjectMethods , ?nonNullable = nonNullable, ?isErased = isErased, ?isSealed = isSealed, ?isInterface = isInterface)
        
    do typeof<MyRecordType>.GetCustomAttributesData()
       |> Seq.iter this.AddCustomAttribute
    
    let compilationMappingFieldConstructor = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>; typeof<int>|])
    let makeCompilationMappingForFields fieldIndex =
        { new CustomAttributeData() with
            member __.Constructor = compilationMappingFieldConstructor
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument SourceConstructFlags.Field; CustomAttributeTypedArgument fieldIndex |]
            member __.NamedArguments = upcast [| |] 
        }

    let buildFieldsAssignmentExpression fields args =
        let rec buildSequentialExpression this =
            function
            | [] -> <@@ () @@>
            | (field, arg)  :: t ->
                Quotations.Expr.Sequential(
                    Quotations.Expr.FieldSetUnchecked(this, field, arg),
                    buildSequentialExpression this t
                )
        match args with
        | [] -> invalidArg "args" "args is not supposed to be empty"
        | this :: args ->
            buildSequentialExpression this (List.zip fields args)

    do this.AddInterfaceImplementation (typedefof<IComparable>)
    do this.AddMember(
            ProvidedMethod(
                "CompareTo", 
                [ProvidedParameter("obj", typeof<Object>)],
                typeof<int>,
                invokeCode = (fun args -> <@@ 0 @@> )
            ) 
        )
    do this.AddInterfaceImplementation (typedefof<IComparable<_>>.MakeGenericType(this))
    do this.AddMember(
            ProvidedMethod(
                "CompareTo", 
                [ProvidedParameter("obj", this)],
                typeof<int>,
                invokeCode = (fun args -> <@@ 0 @@> )
            ) 
        )
    do this.AddInterfaceImplementation (typedefof<System.Collections.IStructuralComparable>)
    do this.AddMember(
            ProvidedMethod(
                "CompareTo", 
                [ProvidedParameter("obj", this); ProvidedParameter("comparer", typeof<System.Collections.IComparer>)],
                typeof<int>,
                invokeCode = (fun args -> <@@ 0 @@> )
            ) 
        )
    //do this.AddInterfaceImplementation (typedefof<System.Collections.IStructuralEquatable>)    

    //do this.AddInterfaceImplementation (typedefof<IEquatable<_>>.MakeGenericType(this))
    
    
    
    let providedTypeMembers =
        recordFields
        |> Seq.mapi (fun index (fieldName, fieldType) ->
            if isNull fieldType then
                failwithf "Unknown column type: %A" fieldType
            let field = ProvidedField("_" + fieldName, fieldType)
            
            let getterCode = Some (fun (args:list<Quotations.Expr>) -> Quotations.Expr.FieldGetUnchecked(args.[0], field))
            let pattrs = enum<MethodAttributes>(0) ||| MethodAttributes.Public ||| MethodAttributes.SpecialName
            let getter = getterCode |> Option.map (fun _ -> ProvidedMethod(false, "get_" + fieldName, pattrs, [||], fieldType, getterCode, [], None, fun _ ->  [| |] ) :> MethodInfo)
            
            let property =
                ProvidedProperty(
                    false,
                    fieldName,
                    PropertyAttributes.None,
                    fieldType,
                    false,
                    getter |> Option.map (fun gt -> (fun () -> gt)) ,
                    None,
                    [| |],
                    fun _ -> [| makeCompilationMappingForFields index |]
                )

            field, property
        ) |> Seq.toList

    do providedTypeMembers
        |> Seq.iter (fun (field, property) ->
            this.AddMember field
            this.AddMember property
        )
    let constructorParameters = providedTypeMembers |> Seq.map (fun (f,p) -> f, ProvidedParameter(p.Name, p.PropertyType)) |> Seq.toList

    let invoke = buildFieldsAssignmentExpression (providedTypeMembers |> List.map fst)

    do this.AddMember <|  ProvidedConstructor(constructorParameters |> List.map snd, invokeCode = invoke )