namespace ProviderImplementation.ProvidedTypes.Extensions
open System
open System.Reflection
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

module Utils = 
    let makeSerializableAttribute () = 
        { new CustomAttributeData() with
            member __.Constructor = typeof<SerializableAttribute>.GetConstructor([||])
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument SourceConstructFlags.RecordType |]
            member __.NamedArguments = upcast [| |] 
        }

    let compilationMappingRecordTypeConstructor = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>|])
    let makeCompilationMappingAttribute() = 
        { new CustomAttributeData() with
            member __.Constructor = compilationMappingRecordTypeConstructor
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument SourceConstructFlags.RecordType |]
            member __.NamedArguments = upcast [| |] 
        }

    let compilationMappingFieldConstructor = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>; typeof<int>|])
    let makeCompilationMappingForFields fieldIndex =
        { new CustomAttributeData() with
            member __.Constructor = compilationMappingFieldConstructor
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument SourceConstructFlags.Field; CustomAttributeTypedArgument fieldIndex |]
            member __.NamedArguments = upcast [| |] 
        }

    let compileGeneratedAttributeConstructor = typeof<System.Runtime.CompilerServices.CompilerGeneratedAttribute>.GetConstructor([||])
    let makeCompilerGeneratedAttribute () = 
        { new CustomAttributeData() with
            member __.Constructor = compileGeneratedAttributeConstructor
            member __.ConstructorArguments = upcast [||]
            member __.NamedArguments = upcast [| |] 
        }

    let debuggerBrowsableAttributeConstructor = typeof<System.Diagnostics.DebuggerBrowsableAttribute>.GetConstructor [|typeof<System.Diagnostics.DebuggerBrowsableState>|]
    let makeDebuggerBrowsableAttribute () =
        { new CustomAttributeData() with
            member __.Constructor = debuggerBrowsableAttributeConstructor
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument System.Diagnostics.DebuggerBrowsableState.Never|]
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

    let coerceToObj expr = Expr.Coerce(expr, typeof<System.Object>)
    let coerce<'T> expr = Expr.Coerce(expr, typeof<'T>)
    let operatorsType = 
        match <@ 1 + 1 @> with 
        | Quotations.Patterns.Call (_, op, _) -> op.DeclaringType
        | _ -> failwith "Unexpected result for the quotation"

    let opEquality typ = operatorsType.GetMethod("op_Equality").MakeGenericMethod([|typ|])
    let opGreaterThan typ = operatorsType.GetMethod("op_GreaterThan").MakeGenericMethod([|typ|])
    let buildGetHashCode self (fields:ProvidedField list) =
        let getHashCodeMethod (t:Type) = t.GetMethod("GetHashCode")
        (fields,<@ 0 @>)
        ||> Seq.foldBack(fun field hashCode -> 
            let hashCodeMethod = getHashCodeMethod field.FieldType
            let selfField = Quotations.Expr.FieldGet (self, field)
            <@ 
                %%Quotations.Expr.Call(selfField, hashCodeMethod, []) + ((%hashCode <<< 6) + (%hashCode >>> 2)) - 1640531527
            @>)
        
    let buildEquals typ self other fields =
        let rec comparefields (fields:ProvidedField list) = 
            match fields with 
            | [] -> <@ false @>
            | field::t -> 
                if typedefof<IEquatable<_>>.MakeGenericType([|field.FieldType|]).IsAssignableFrom(field.FieldType) then
                    let selfField = Quotations.Expr.FieldGet (self, field)
                    let otherField = Quotations.Expr.FieldGet (other, field)
                    <@
                        if %%Quotations.Expr.Call(opEquality field.FieldType, [selfField; otherField]) then
                            %(comparefields t)
                        else 
                            false
                    @>
                else 
                    <@ false @>

        <@@
            if %%(coerceToObj self) = null then
                %%(coerceToObj other) = null
            else
                %%(coerceToObj other) <> null && %(comparefields fields)
        @@>
    let buildCompareTo typ self other fields =
        let rec comparefields (fields:ProvidedField list) = 
            match fields with 
            | [] -> <@ 0 @>
            | field::t -> 
                if typeof<IComparable>.IsAssignableFrom(field.FieldType) then
                    let selfField = Quotations.Expr.FieldGet (self, field)
                    let otherField = Quotations.Expr.FieldGet (other, field)
                    <@
                        if %%Quotations.Expr.Call(opEquality field.FieldType, [selfField; otherField]) then
                            %(comparefields t)
                        elif %%Quotations.Expr.Call(opGreaterThan field.FieldType, [selfField; otherField])  then
                            1
                        else
                            -1
                    @>
                else 
                    <@ -1 @>

        <@@
            if %%(coerceToObj self) = null then
                if %%(coerceToObj other) = null then 0 else -1
            else
                if %%(coerceToObj other) = null then 1 else %(comparefields fields)
        @@>

type ProvidedMethodWithAttributes(isTgt: bool, methodName: string, attrs: MethodAttributes, parameters: ProvidedParameter[], returnType: Type, invokeCode: (Expr list -> Expr) option, staticParams, staticParamsApply, customAttributesData) =
    inherit ProvidedMethod(isTgt, methodName, attrs, parameters, returnType, invokeCode, staticParams, staticParamsApply, customAttributesData)
    new (methodName, attrs, parameters, returnType, customAttributes, ?invokeCode, ?isStatic) =
       ProvidedMethodWithAttributes(false, methodName, attrs, Array.ofList parameters, returnType, invokeCode, [], None, K customAttributes)

type ProvidedRecordDefnition (isTgt: bool, container:TypeContainer, className: string, recordFields:seq<string*Type>, getBaseType: (unit -> Type option), attrs: TypeAttributes, getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods) as this =
    inherit ProvidedTypeDefinition(isTgt, container, className, getBaseType, attrs, getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods)
    do
        [
            Utils.makeSerializableAttribute ()
            Utils.makeCompilationMappingAttribute()
        ]
        |> Seq.iter this.AddCustomAttribute

    let providedTypeMembers =
        recordFields
        |> Seq.mapi (fun index (fieldName, fieldType) ->
            if isNull fieldType then
                failwithf "Unknown column type: %A" fieldType
            let field = ProvidedField(false, sprintf "%s@" fieldName, FieldAttributes.Assembly, fieldType, null, (K [| Utils.makeDebuggerBrowsableAttribute() |]))
            
            let getterCode = fun (args:list<Quotations.Expr>) -> Quotations.Expr.FieldGetUnchecked(args.[0], field)
            let pattrs = enum<MethodAttributes>(0) ||| MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig
            let getter = (Some getterCode) |> Option.map (fun _ -> ProvidedMethod(false, "get_" + fieldName, pattrs, [||], fieldType, Some getterCode, [], None, fun _ ->  [| |] ) :> MethodInfo)

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
                    K [| Utils.makeCompilationMappingForFields index |]
                )
            field, property
        ) |> Seq.toList

    do providedTypeMembers
        |> Seq.iter (fun (field, property) ->
            this.AddMember field
            this.AddMember property
        )

    do this.AddInterfaceImplementation (typedefof<IComparable<_>>.MakeGenericType(this))
    do this.AddMember(
            ProvidedMethodWithAttributes(
                "CompareTo", 
                MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
                [ProvidedParameter("other", this)],
                typeof<int>,
                [| Utils.makeCompilerGeneratedAttribute() |],
                invokeCode = (fun args -> 
                    Utils.buildCompareTo this args.[0] args.[1] (providedTypeMembers |> List.map fst))
            ) 
        )
    
    do this.AddInterfaceImplementation (typedefof<IComparable>)
    do this.AddMember(
            ProvidedMethodWithAttributes(
                "CompareTo", 
                MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
                [ProvidedParameter("obj", typeof<Object>)],
                typeof<int>,
                [| Utils.makeCompilerGeneratedAttribute() |],
                invokeCode = (fun args -> 
                    Utils.buildCompareTo this args.[0] (Expr.Coerce(args.[1], this)) (providedTypeMembers |> List.map fst)
                )
            ) 
        )
    do this.AddInterfaceImplementation (typedefof<System.Collections.IStructuralComparable>)
    do this.AddMember(
            ProvidedMethodWithAttributes(
                "CompareTo", 
                MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
                [ProvidedParameter("other", typeof<Object>); ProvidedParameter("comparer", typeof<System.Collections.IComparer>)],
                typeof<int>,
                [| Utils.makeCompilerGeneratedAttribute() |],
                invokeCode = (fun args -> 
                    Utils.buildCompareTo this args.[0] (Expr.Coerce(args.[1], this)) (providedTypeMembers |> List.map fst) 
                )
            ) 
        )
    do this.AddInterfaceImplementation (typedefof<System.Collections.IStructuralEquatable>)
    do this.AddMember(
        ProvidedMethodWithAttributes(
            "Equals", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("other", typeof<obj>); ProvidedParameter("comparer", typeof<System.Collections.IEqualityComparer>)],
            typeof<bool>,
            [| Utils.makeCompilerGeneratedAttribute() |],
            invokeCode = (fun args -> 
                Utils.buildEquals this args.[0] (Expr.Coerce(args.[1], this)) (providedTypeMembers |> List.map fst) 
            )
        ) 
    )

    do this.AddInterfaceImplementation (typedefof<IEquatable<_>>.MakeGenericType(this))
    do this.AddMember(
        ProvidedMethodWithAttributes(
            "Equals", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("other", this)],
            typeof<bool>,   
            [| Utils.makeCompilerGeneratedAttribute() |],
            invokeCode = (fun args -> 
                Utils.buildEquals this args.[0] args.[1] (providedTypeMembers |> List.map fst) 
            )
        ) 
    )

    do this.AddMember(
        ProvidedMethodWithAttributes(
            "Equals", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("other", typeof<obj>)],
            typeof<bool>,   
            [| Utils.makeCompilerGeneratedAttribute() |],
            invokeCode = (fun args -> 
                Utils.buildEquals this args.[0] (Expr.Coerce(args.[1], this)) (providedTypeMembers |> List.map fst) 
            )
        ) 
    )

    do this.AddMember(
        ProvidedMethodWithAttributes(
            "GetHashCode", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("comparer", typeof<System.Collections.IEqualityComparer>)],
            typeof<int>,
            [| Utils.makeCompilerGeneratedAttribute() |],
            invokeCode = (fun args -> Utils.buildGetHashCode args.[0] (providedTypeMembers |> List.map fst) :> Quotations.Expr )
        )) 


    let constructorParameters = providedTypeMembers |> Seq.map (fun (f,p) -> f, ProvidedParameter(p.Name, p.PropertyType)) |> Seq.toList |> List.map snd

    do this.AddMember(
        ProvidedConstructor(
            false, MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName, 
            Array.ofList constructorParameters, 
            Utils.buildFieldsAssignmentExpression (providedTypeMembers |> List.map fst), 
            None, 
            false, 
            K [| |]))

    new (assembly:Assembly, namespaceName, className:string, recordFields :seq<string*Type>) =
        let attrs = 
            TypeAttributes.Public 
            ||| TypeAttributes.Class 
            ||| TypeAttributes.AutoLayout 
            ||| TypeAttributes.AnsiClass 
            ||| TypeAttributes.NestedPublic   
            ||| TypeAttributes.Sealed 
            ||| TypeAttributes.Serializable
        ProvidedRecordDefnition(false, TypeContainer.Namespace (K assembly,namespaceName), className, recordFields,  K (Some typeof<obj>), attrs, K None, [], None, None, K [| |], nonNullable = false, hideObjectMethods = false)