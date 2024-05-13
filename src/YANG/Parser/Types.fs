// Type.fs
// Definitions and parsing for YANG defined types

namespace Yang.Parser

/// Type definitions and parsers for types used in YANG
module Types =
    open FParsec
    open NLog
    open Yang.Model
    //open System.Net.Configuration

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

    let private throw fmt =
        let do_throw message = raise (YangParserException message)
        Printf.ksprintf do_throw fmt

#if INTERACTIVE
    // The following are used only in interactive (fsi) to help with enabling disabling
    // logging for particular modules.

    type internal Marker = interface end
    let _full_name = typeof<Marker>.DeclaringType.FullName
    let _name = typeof<Marker>.DeclaringType.Name
#endif

    type TypeParsers<'a> = {
        Type        : Parser<TypeStatement, 'a>
    }

    let private parsers<'a> : TypeParsers<'a> =
        // Forward definition of type statement; we need this to parse the union type
        let (parse_type_statement : Parser<TypeStatement, 'a>), (parse_type_statement_ref : Parser<TypeStatement, 'a> ref) =
            createParserForwardedToRef<TypeStatement, 'a>()

        //
        // Parsers for the specific types
        //

        /// Parse restrictions for the string type
        let parse_type_body_string_restrictions : Parser<StringRestrictions, 'a> =
            // [RFC 7950, p. 189]
            //string-restrictions = ;; these stmts can appear in any order
            //                        [length-stmt]
            //                        *pattern-stmt
            // TODO: Check and enforce cardinality of string-restrictions
            many (
                    (parse_length_statement     |>> StringBodyRestrictions.Length)
                <|> (parse_pattern_statement    |>> StringBodyRestrictions.Pattern)
                <|> (parse_unknown_statement    |>> StringBodyRestrictions.Unknown)
                .>> wse
            )

        let parse_enum_specification : Parser<EnumSpecification, 'a> =
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
            // TODO: Check and enforce cardinality of enum-specification
            many1 (
                    (parse_enum_statement       |>> EnumBodySpecification.Enum)
                <|> (parse_unknown_statement    |>> EnumBodySpecification.Unknown)
                .>> wse
            )

        let parse_leaf_ref_body_specification : Parser<LeafRefBodySpecification, 'a> =
                (parse_path_statement               |>> LeafRefBodySpecification.Path)
            <|> (parse_require_instance_statement   |>> LeafRefBodySpecification.Require)
            <|> (parse_unknown_statement            |>> LeafRefBodySpecification.Unknown)

        let parse_leaf_ref_specification : Parser<LeafRefSpecification, 'a> =
            // [RFC 7950, p. 190]
            //leafref-specification =
            //                        ;; these stmts can appear in any order
            //                        path-stmt
            //                        [require-instance-stmt]
            // TODO: Check and enforce cardinality of leafref-specification
            many1 (parse_leaf_ref_body_specification .>> wse)

        let parse_type_body_numerical_restrictions : Parser<NumericalRestrictions, 'a> =
            // [RFC 7950, p. 189]
            // numerical-restrictions = [range-stmt]
            // TODO: Check and enforce cardinality of numerical-restrictions
            many1 (
                    (parse_range_statement      |>> NumericalBodyRestrictions.Range)
                <|> (parse_unknown_statement    |>> NumericalBodyRestrictions.Unknown)
                 .>> wse
            )

        let parse_type_decimal64_body_restrictions : Parser<Decimal64BodySpecification, 'a> =
                (parse_fraction_digits_statement    |>> Decimal64BodySpecification.FractionDigits)
            <|> (parse_range_statement              |>> Decimal64BodySpecification.Range)
            <|> (parse_unknown_statement            |>> Decimal64BodySpecification.Unknown)

        let parse_type_decimal64_restrictions : Parser<Decimal64Specification, 'a> =
            // [RFC 7950, p. 189]
            //decimal64-specification = ;; these stmts can appear in any order
            //                            fraction-digits-stmt
            //                            [range-stmt]
            many1 (parse_type_decimal64_body_restrictions .>> wse)

        let parse_identity_ref_specification : Parser<IdentityRefSpecification, 'a> =
            // [RFC 7950, p. 189]
            //identityref-specification =
            //                        1*base-stmt
            many1 (
                    (parse_base_statement       |>> IdentityRefBodySpecification.Base)
                <|> (parse_unknown_statement    |>> IdentityRefBodySpecification.Unknown)
                .>> wse
            )

        let parse_union_specification : Parser<UnionSpecification, 'a> =
            // [RFC 7950, p. 191]
            //union-specification = 1*type-stmt
            many1 (
                    (parse_type_statement       |>> UnionBodySpecification.Type)
                <|> (parse_unknown_statement    |>> UnionBodySpecification.Unknown)
                .>> wse
            )

        let parse_binary_specification : Parser<BinarySpecification, 'a> =
            // [RFC 7950, p. 191]
            //binary-specification = [length-stmt]
            many (
                    (parse_length_statement     |>> BinaryBodySpecification.Length)
                <|> (parse_unknown_statement    |>> BinaryBodySpecification.Unknown)
                .>> wse
            )

        let parse_instance_identifier_specification : Parser<InstanceIdentifierSpecification, 'a> =
            // [RFC 7950, p. 191]
            //instance-identifier-specification =
            //                        [require-instance-stmt]
            many (
                    (parse_require_instance_statement   |>> InstanceIdentifierBodySpecification.RequireInstance)
                <|> (parse_unknown_statement            |>> InstanceIdentifierBodySpecification.Unknown)
                .>> wse
            )

        let parse_bit_specification : Parser<BitsSpecification, 'a> =
            // [RFC 7950, p. 191]
            //bits-specification  = 1*bit-stmt
            many1 (
                    (parse_bit_statement            |>> BitsBodySpecification.Bit)
                <|> (parse_unknown_statement        |>> BitsBodySpecification.Unknown)
                .>> wse
            )

        //
        // End of parsers for the specific types
        //

        /// Parses a specific string and also returns it in the parser output
        let force_type_name (``type`` : string) : Parser<IdentifierReference, 'a> =
            // The type name should end with whitespace or ';'
            attempt (
                    (pstring ``type`` .>> (spaces1 <|> (followedBy (skipChar ';'))))
                <|> (pip_pstring Strings.parse_string ``type``)
            )
            |>> fun _ -> Yang.Model.Identifier.IdentifierReference.Make ``type``

        /// Resolve the parser for the type specific restriction statements
        let parse_type_body_restriction_statement (``type`` : string) : Parser<TypeBodyStatement, 'a> =
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
                    ("leafref",     parse_leaf_ref_specification            |>> TypeBodyStatement.LeafRefSpecification)
                    ("union",       parse_union_specification               |>> TypeBodyStatement.UnionSpecification)
                    ("identityref", parse_identity_ref_specification        |>> TypeBodyStatement.IdentityRefSpecification)
                    ("binary",      parse_binary_specification              |>> TypeBodyStatement.BinarySpecification)
                    ("bits",        parse_bit_specification                 |>> TypeBodyStatement.BitsSpecification)
                    ("instance-identifier", parse_instance_identifier_specification |>> TypeBodyStatement.InstanceIdentifierSpecification)
                ]

            if (restrictions.ContainsKey ``type``) = false then
                throw "Restrictions for type %s not implemented yet" ``type``
            else
                restrictions.Item ``type``

        /// Type-specific parser that implements the logic of parsing the specified parser and the appropriate parser restrictions.
        let parse_type_implementation_statement (``type`` : string) : Parser<(TypeBodyStatement option), 'a> =
                (skipChar ';' >>. wse   |>> fun _ -> None)
            <|> (skipChar '{' >>. wse >>.
                 (parse_type_body_restriction_statement ``type``) .>>
                 skipChar '}' .>> wse   |>> Some
                )

        /// Parses a type statement
        let parse_type_statement_implementation : Parser<TypeStatement, 'a> =
            let parse_all_unknowns = many parse_statement
            let parse_end_of_unknown_type =
                    (skipChar ';' >>. wse   |>> fun _ -> None)
                <|> (skipChar '{' >>. wse >>.
                     parse_all_unknowns .>>
                     skipChar '}' .>> wse
                     |>> fun unknowns ->
                            if unknowns.Length = 0 then None
                            else Some unknowns
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
                <|> do_parse "leafref"
                <|> do_parse "identityref"
                <|> do_parse "union"
                <|> do_parse "binary"
                <|> do_parse "bits"
                <|> do_parse "instance-identifier"
                <|> (parse_identifier_reference .>> spaces .>>. parse_end_of_unknown_type
                     |>> fun (id, spec) ->
                        if spec.IsNone then id, None
                        else id, Some (TypeBodyStatement.UnknownTypeSpecification spec.Value)
                    )
                |>> TypeStatement
            )

        parse_type_statement_ref := parse_type_statement_implementation

        {
            Type = parse_type_statement
        }

    let parse_type_statement<'a> : Parser<TypeStatement, 'a> = parsers.Type
