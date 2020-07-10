namespace ProviderImplementation.ProvidedTypes.Extensions
open System
open System.Reflection
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

[<AutoOpen>]
module Misc = 
    let mkSerializableAttribute () = 
        { new CustomAttributeData() with
            member __.Constructor = typeof<SerializableAttribute>.GetConstructor([||])
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument SourceConstructFlags.RecordType |]
            member __.NamedArguments = upcast [| |] 
        }

    let compilationMappingRecordTypeConstructor = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>|])
    let mkCompilationMappingAttribute() = 
        { new CustomAttributeData() with
            member __.Constructor = compilationMappingRecordTypeConstructor
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument SourceConstructFlags.RecordType |]
            member __.NamedArguments = upcast [| |] 
        }

    let compilationMappingFieldConstructor = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>; typeof<int>|])
    let mkCompilationMappingForFields fieldIndex =
        { new CustomAttributeData() with
            member __.Constructor = compilationMappingFieldConstructor
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument SourceConstructFlags.Field; CustomAttributeTypedArgument fieldIndex |]
            member __.NamedArguments = upcast [| |] 
        }

    let compileGeneratedAttributeConstructor = typeof<System.Runtime.CompilerServices.CompilerGeneratedAttribute>.GetConstructor([||])
    let mkCompilerGeneratedAttribute () = 
        { new CustomAttributeData() with
            member __.Constructor = compileGeneratedAttributeConstructor
            member __.ConstructorArguments = upcast [||]
            member __.NamedArguments = upcast [| |] 
        }

    let debuggerBrowsableAttributeConstructor = typeof<System.Diagnostics.DebuggerBrowsableAttribute>.GetConstructor [|typeof<System.Diagnostics.DebuggerBrowsableState>|]
    let mkDebuggerBrowsableAttribute () =
        { new CustomAttributeData() with
            member __.Constructor = debuggerBrowsableAttributeConstructor
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument System.Diagnostics.DebuggerBrowsableState.Never|]
            member __.NamedArguments = upcast [| |] 
        }

type FieldDefinition = {
    Name: string
    Description: string
    FieldType: Type
}


type ProvidedRecordDefnition (isTgt: bool, container:TypeContainer, className: string, recordFields:seq<FieldDefinition>, getBaseType: (unit -> Type option), attrs: TypeAttributes, getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods) as this =
    inherit ProvidedTypeDefinition(isTgt, container, className, getBaseType, attrs, getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods)
    do
        [
            mkSerializableAttribute ()
            mkCompilationMappingAttribute()
        ]
        |> Seq.iter this.AddCustomAttribute

    let createProvidedMethod(methodName, attrs, parameters, returnType, customAttributes, invokeCode) =
        ProvidedMethod(false, methodName, attrs, Array.ofList parameters, returnType, Some invokeCode, [], None, K customAttributes)

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
    
    let buildEquals self other fields =
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
    
    let buildCompareTo self other fields =
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

    let providedTypeMembers =
        recordFields
        |> Seq.mapi (fun index {Name = fieldName; FieldType = fieldType; Description = doc } ->
            if isNull fieldType then
                failwithf "Unknown column type: %A" fieldType
            let field = ProvidedField(false, sprintf "%s@" fieldName, FieldAttributes.Assembly, fieldType, null, (K [| mkDebuggerBrowsableAttribute() |]))
            
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
                    getter |> Option.map (fun gt -> (fun () -> gt)),
                    None,
                    [| |],
                    K [| mkCompilationMappingForFields index |]
                )
            property.AddXmlDoc doc
            field, property
        ) |> Seq.toList

    let providedFields = providedTypeMembers |> List.map fst

    do providedTypeMembers
        |> Seq.iter (fun (field, property) ->
            this.AddMember field
            this.AddMember property
        )

    do this.AddInterfaceImplementation (typedefof<IComparable<_>>.MakeGenericType(this))
    do this.AddMember(
        createProvidedMethod(
            "CompareTo", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("other", this)],
            typeof<int>,
            [| mkCompilerGeneratedAttribute() |],
            (fun args -> buildCompareTo args.[0] args.[1] providedFields) 
        ) 
    )
    
    do this.AddInterfaceImplementation (typedefof<IComparable>)
    do this.AddMember(
        createProvidedMethod(
            "CompareTo", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("obj", typeof<Object>)],
            typeof<int>,
            [| mkCompilerGeneratedAttribute() |],
            (fun args -> buildCompareTo args.[0] (Expr.Coerce(args.[1], this)) providedFields)
        ) 
    )
    do this.AddInterfaceImplementation (typedefof<System.Collections.IStructuralComparable>)
    do this.AddMember(
        createProvidedMethod(
            "CompareTo", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("other", typeof<Object>); ProvidedParameter("comparer", typeof<System.Collections.IComparer>)],
            typeof<int>,
            [| mkCompilerGeneratedAttribute() |],
            (fun args -> buildCompareTo args.[0] (Expr.Coerce(args.[1], this)) providedFields)
        ) 
    )
    do this.AddInterfaceImplementation (typedefof<System.Collections.IStructuralEquatable>)
    do this.AddMember(
        createProvidedMethod(
            "Equals", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("other", typeof<obj>); ProvidedParameter("comparer", typeof<System.Collections.IEqualityComparer>)],
            typeof<bool>,
            [| mkCompilerGeneratedAttribute() |],
            (fun args -> buildEquals args.[0] (Expr.Coerce(args.[1], this)) providedFields)
        ) 
    )

    do this.AddInterfaceImplementation (typedefof<IEquatable<_>>.MakeGenericType(this))
    do this.AddMember(
        createProvidedMethod(
            "Equals", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("other", this)],
            typeof<bool>,   
            [| mkCompilerGeneratedAttribute() |],
            (fun args -> buildEquals args.[0] args.[1] providedFields)
        ) 
    )

    do this.AddMember(
        createProvidedMethod(
            "Equals", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("other", typeof<obj>)],
            typeof<bool>,   
            [| mkCompilerGeneratedAttribute() |],
            (fun args -> buildEquals args.[0] (Expr.Coerce(args.[1], this)) providedFields)
        ) 
    )

    do this.AddMember(
        createProvidedMethod(
            "GetHashCode", 
            MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual  ||| MethodAttributes.HideBySig,
            [ProvidedParameter("comparer", typeof<System.Collections.IEqualityComparer>)],
            typeof<int>,
            [| mkCompilerGeneratedAttribute() |],
            (fun args -> buildGetHashCode args.[0] providedFields :> Quotations.Expr )
        )) 

    let constructorParameters = providedTypeMembers |> Seq.map (fun (f,p) -> f, ProvidedParameter(p.Name, p.PropertyType)) |> Seq.toList |> List.map snd

    do this.AddMember(
        ProvidedConstructor(
            false, MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName, 
            Array.ofList constructorParameters, 
            buildFieldsAssignmentExpression providedFields, 
            None, 
            false, 
            K [| |]))

    new (assembly:Assembly, namespaceName, className:string, recordFields :seq<FieldDefinition>) =
        let attrs = 
            TypeAttributes.Public 
            ||| TypeAttributes.Class 
            ||| TypeAttributes.AutoLayout 
            ||| TypeAttributes.AnsiClass 
            ||| TypeAttributes.NestedPublic   
            ||| TypeAttributes.Sealed 
            ||| TypeAttributes.Serializable
        ProvidedRecordDefnition(false, TypeContainer.Namespace (K assembly,namespaceName), className, recordFields,  K (Some typeof<obj>), attrs, K None, [], None, None, K [| |], nonNullable = false, hideObjectMethods = false)