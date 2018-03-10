// Type.fs
// Definitions and parsing for YANG defined types

namespace Yang.Parser

/// Type definitions and parsers for types used in YANG
module Types =
    open FParsec
    open NLog
    open Yang.Model
    open System.Net.Configuration

    // [RFC 7950, p.188]
    //   type-stmt           = type-keyword sep identifier-ref-arg-str optsep
    //                         (";" /
    //                          "{" stmtsep
    //                              [type-body-stmts]
    //                          "}") stmtsep
    //   type-body-stmts     = numerical-restrictions /
    //                         decimal64-specification /
    //                         string-restrictions /
    //                         enum-specification /
    //                         leafref-specification /
    //                         identityref-specification /
    //                         instance-identifier-specification /
    //                         bits-specification /
    //                         union-specification /
    //                         binary-specification
    //
    // For a discussion of build-in types see [RFC 7950, Sec. 4.2.4 and Sec. 9]
    // There are a number of common types defined in [RFC 6991]
    //
    // Built-in types from [RFC 7950, p. 24]
    //+---------------------+-------------------------------------+
    //| Name                | Description                         |
    //+---------------------+-------------------------------------+
    //| binary              | Any binary data                     |
    //| bits                | A set of bits or flags              |
    //| boolean             | "true" or "false"                   |
    //| decimal64           | 64-bit signed decimal number        |
    //| empty               | A leaf that does not have any value |
    //| enumeration         | One of an enumerated set of strings |
    //| identityref         | A reference to an abstract identity |
    //| instance-identifier | A reference to a data tree node     |
    //| int8                | 8-bit signed integer                |
    //| int16               | 16-bit signed integer               |
    //| int32               | 32-bit signed integer               |
    //| int64               | 64-bit signed integer               |
    //| leafref             | A reference to a leaf instance      |
    //| string              | A character string                  |
    //| uint8               | 8-bit unsigned integer              |
    //| uint16              | 16-bit unsigned integer             |
    //| uint32              | 32-bit unsigned integer             |
    //| uint64              | 64-bit unsigned integer             |
    //| union               | Choice of member types              |
    //+---------------------+-------------------------------------+

    // TODO: Add all common types
    // TODO: Define types for type restrictions and specifications
    // TODO: Enrich parser to handle custom types

    /// Logger for this module
    let private _logger = LogManager.GetCurrentClassLogger()

#if INTERACTIVE
    // The following are used only in interactive (fsi) to help with enabling disabling
    // logging for particular modules.

    type internal Marker = interface end
    let _full_name = typeof<Marker>.DeclaringType.FullName
    let _name = typeof<Marker>.DeclaringType.Name
#endif

    /// Parse restrictions for the string type
    let private  parse_type_body_string_restrictions<'a> : Parser<StringRestrictions, 'a> =
        // [RFC 7950, p. 189]
        //string-restrictions = ;; these stmts can appear in any order
        //                        [length-stmt]
        //                        *pattern-stmt

            (parse_length_statement     |>> fun v -> Some v, [])
        <|> (parse_pattern_statement    |>> fun v -> None,   [v])

    let private parse_enum_specification<'a> : Parser<EnumSpecification, 'a> =
        // [RFC 7950, p. 190]
        //enum-specification  = 1*enum-stmt
        //enum-stmt           = enum-keyword sep string optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            *if-feature-stmt
        //                            [value-stmt]
        //                            [status-stmt]
        //                            [description-stmt]
        //                            [reference-stmt]
        //                        "}") stmtsep
        many1 parse_enum_statement

    let private parse_type_body_numerical_restrictions<'a> : Parser<NumericalRestrictions, 'a> =
        // [RFC 7950, p. 189]
        // numerical-restrictions = [range-stmt]
        parse_range_statement

    let private parse_type_decimal64_restrictions<'a> : Parser<Decimal64Specification, 'a> =
        // [RFC 7950, p. 189]
        //decimal64-specification = ;; these stmts can appear in any order
        //                            fraction-digits-stmt
        //                            [range-stmt]
            (parse_fraction_digits_statement    |>> (fun v -> v, None))
        <|> (parse_range_statement              |>> (fun r -> (System.Byte.MaxValue, None), Some r))

    /// Resolve the parser for the type specific restriction statements
    let private parse_type_body_restriction_statement<'a> (``type`` : string) : Parser<TypeBodyStatement, 'a> =
        // TODO: Add type restrictions for all internal types
        // TODO: Extensibility mechanism for custom type restrictions

        let restrictions =
            Map.ofList [
                ("string",      parse_type_body_string_restrictions     |>> TypeBodyStatement.StringRestrictions)
                ("enumeration", parse_enum_specification                |>> TypeBodyStatement.EnumSpecification)
                ("int8",        parse_type_body_numerical_restrictions  |>> TypeBodyStatement.NumericalRestrictions)
                ("int16",       parse_type_body_numerical_restrictions  |>> TypeBodyStatement.NumericalRestrictions)
                ("int32",       parse_type_body_numerical_restrictions  |>> TypeBodyStatement.NumericalRestrictions)
                ("int64",       parse_type_body_numerical_restrictions  |>> TypeBodyStatement.NumericalRestrictions)
                ("uint8",       parse_type_body_numerical_restrictions  |>> TypeBodyStatement.NumericalRestrictions)
                ("uint16",      parse_type_body_numerical_restrictions  |>> TypeBodyStatement.NumericalRestrictions)
                ("uint32",      parse_type_body_numerical_restrictions  |>> TypeBodyStatement.NumericalRestrictions)
                ("uint64",      parse_type_body_numerical_restrictions  |>> TypeBodyStatement.NumericalRestrictions)
                ("decimal64",   parse_type_decimal64_restrictions       |>> TypeBodyStatement.Decimal64Specification)
            ]

        if (restrictions.ContainsKey ``type``) = false then
            raise (YangParserException (sprintf "Restrictions for type %s not implemented yet" ``type``))
        else
            restrictions.Item ``type``

    /// Parses a specific string and also returns it in the parser output
    let private force_type_name<'a> (``type`` : string) : Parser<IdentifierReference, 'a> =
        pstring ``type`` .>> spaces
        |>> fun v -> Yang.Model.Identifier.IdentifierReference.Make ``type``

    /// Transforms a list of type restrictions into a single type restriction
    let translate_type_restrictions (``type`` : string) (restrictions : TypeBodyStatement list) : TypeBodyStatement option =
        // REFACTOR: The type restrictions should be broken to separate functions per type.

        if ``type``.Equals("string") then
            let result =
                restrictions
                |> List.fold (
                    fun state restriction ->
                        match state, restriction with
                        | (None, patterns), StringRestrictions (Some length, [])    -> Some length, patterns
                        | (None, _), StringRestrictions (Some length, _)            ->
                            raise (YangParserException "Internal error: expected either length restriction or pattern; got both")
                        | (Some _, _),      StringRestrictions (Some _, _)          ->
                            raise (YangParserException "Error: only one length restriction statement allowed")
                        | (length, patterns), StringRestrictions (None, pattern)    -> length, patterns @ pattern
                        | _ ->
                            raise (YangParserException "Only string restrictions allowed for string type")
                ) (None, [])

            match result  with
            | None, []      -> None
            | _             -> Some (TypeBodyStatement.StringRestrictions result)

        elif ``type``.Equals("enumeration") then
            let enumerations =
                restrictions
                |> List.collect (
                    fun enum ->
                        match enum with
                        | TypeBodyStatement.EnumSpecification enums -> enums
                        | _     -> raise (YangParserException (sprintf "Expected enum specification, got %A" enum))
                )

            // TODO: Check that the labels of all enumerations are difference, and write unit test
            if List.length enumerations = 0 then
                raise (YangParserException (sprintf "Enumerations must have at least one case"))

            Some (TypeBodyStatement.EnumSpecification enumerations)

        elif   ``type``.Equals("int8")  || ``type``.Equals("int16")  || ``type``.Equals("int32")  || ``type``.Equals("int64")
            || ``type``.Equals("uint8") || ``type``.Equals("uint16") || ``type``.Equals("uint32") || ``type``.Equals("uint64")
        then
            if List.length restrictions  = 0    then None
            elif List.length restrictions > 1   then raise (YangParserException (sprintf "Only a single numerical restriction is allowed; got: %A" restrictions))
            else
                let restriction = List.head restrictions
                match restriction with
                // TODO: Do we need to make the following check; it should be guaranteed tha the value could be of NumericalRestrictions type
                | TypeBodyStatement.NumericalRestrictions range ->
                    // TODO: Check that the range limits are compatible with the specified type
                    Some restriction
                | _ -> raise (YangParserException (sprintf "Only a single range restriction allowed for numerics; got: %A" restriction))

        elif ``type``.Equals("decimal64") then
            match restrictions with
            | []    ->
                raise (YangParserException (sprintf "Decimal64 type requires at least the specification of the number of fraction digits"))

            | (TypeBodyStatement.Decimal64Specification ((fraction, _), _) as r) :: []   ->
                if fraction = System.Byte.MaxValue then 
                    raise (YangParserException (sprintf "Missing specification of the number of fractional digits"))
                else Some r

            | (TypeBodyStatement.Decimal64Specification r1) :: (TypeBodyStatement.Decimal64Specification r2) :: [] ->
                match r1, r2 with
                | ((System.Byte.MaxValue, _), _), ((System.Byte.MaxValue, _), _) ->
                    raise (YangParserException (sprintf "Missing specification of the number of fractional digits"))

                | ((System.Byte.MaxValue, _), range), (length, None)
                | (length, None), ((System.Byte.MaxValue, _), range)
                    ->
                        let result = Decimal64Specification (length, range)
                        Some result

                | _ -> raise (YangParserException (sprintf "Invalid specification parameters for Decimal64"))

            | _ -> raise (YangParserException (sprintf "Decimal64 type requires one or two specification statements"))

        else
            // TODO: Write type restriction accumulators for all basic types
            failwith (sprintf "Support for type %s not implemented" ``type``)

    type BodyParserState<'T> =
    | InternalUnknown       of UnknownStatement
    | InternalRestriction   of 'T

    /// Parser for the child block that contains the type-specific restrictions
    let private try_parse_type_restrictions<'a, 'T> (restrictions : Parser<'T, 'a>) : Parser<(UnknownStatement list option) * ('T list option), 'a> =
        let element_parser =
                (restrictions               |>> fun restriction -> InternalRestriction restriction)
            <|> (parse_unknown_statement    |>> fun statement   -> InternalUnknown statement)

        let initial = function
        | InternalRestriction   restriction -> None, Some [ restriction ]
        | InternalUnknown       unknown     -> Some [ unknown ], None

        let fold state element =
            match state, element with
            | (unknowns, None),                 InternalRestriction restriction -> unknowns, Some [ restriction ]
            | (unknowns, Some restrictions),    InternalRestriction restriction -> unknowns, Some (restrictions @ [restriction])
            | (None, restrictions),             InternalUnknown unknown         -> Some [ unknown ], restrictions
            | (Some unknowns, restrictions),    InternalUnknown unknown         -> Some (unknowns @ [ unknown ]), restrictions

        ParserHelper.ConsumeMany(
            elementParser           = element_parser,
            stateFromFirstElement   = initial,
            foldState               = fold,
            resultFromState         = id,
            resultForEmptySequence  = fun _ -> None, None
        )


    /// Type-specific parser that implements the logic of parsing the specified parser and the appropriate parser restrictions.
    let private parse_type_implementation_statement<'a> (``type`` : string) : Parser<(TypeBodyStatement option) * (UnknownStatement list option), 'a> =
            (skipChar ';' >>. wse   |>> fun _ -> None, None)
        <|> (skipChar '{' >>. wse >>.
             (try_parse_type_restrictions (parse_type_body_restriction_statement ``type``)) .>>
             skipChar '}' .>> wse
            )
        |>> fun (unknowns, restrictions) ->
                let unknowns' =
                    if unknowns.IsSome && unknowns.Value.Length = 0 then None
                    else unknowns
                if restrictions.IsNone then None, unknowns
                else (translate_type_restrictions ``type`` restrictions.Value), unknowns'

    /// Parses a type statement
    let parse_type_statement<'a> : Parser<TypeStatement, 'a> =
        let parse_all_unknowns = many parse_unknown_statement
        let parse_end_of_unknown_type =
                (skipChar ';' >>. wse   |>> fun _ -> None, None)
            <|> (skipChar '{' >>. wse >>.
                 parse_all_unknowns .>>
                 skipChar '}' .>> wse
                 |>> fun unknowns ->
                        if unknowns.Length = 0 then None, None
                        else None, Some unknowns
                )

        let do_parse type_name =
            force_type_name type_name       .>>. parse_type_implementation_statement type_name

        skipString "type" >>. spaces >>.
        (
                do_parse "string"
            <|> do_parse "enumeration"
            <|> do_parse "int8"
            <|> do_parse "int16"
            <|> do_parse "int32"
            <|> do_parse "int64"
            <|> do_parse "uint8"
            <|> do_parse "uint16"
            <|> do_parse "uint32"
            <|> do_parse "uint64"
            <|> do_parse "decimal64"
            <|> (Identifier.parse_identifier_reference .>>. parse_end_of_unknown_type)
        )
        |>> fun (id, (restriction, unknowns)) -> id, restriction, unknowns
