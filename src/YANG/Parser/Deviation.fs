// Deviation.fs
namespace Yang.Parser

/// Parser for deviation-stmt and deviate-[add|delete|replace]-stmt
[<AutoOpen>]
module Deviation =
    open FParsec
    open Yang.Model
    open Expressions

    //
    // Parsing of deviation statements.
    // Parsing differs slightly than the rest of the statements, because the deviate-[add|delete|replace]-stmt use the same keyword
    //

    // TODO: For the strings below (add/delete/replace) we may want to use the parser-in-parser

    let private parse_deviate_generic_statement<'a, 'b>
        (keyword    : string)
        (body       : Parser<'b, 'a>) : Parser<('b list option), 'a>
        =
            skipString "deviate" >>. spaces >>. skipString keyword >>. spaces >>.
            (
                    (end_of_statement                       |>> (fun _ -> None))
                <|> (begin_block >>. (block_generic body)   |>> Some)
            )

    let parse_deviate_add_body_statement<'a> : Parser<DeviateAddBodyStatement, 'a> =
            (parse_units_statement          |>> DeviateAddBodyStatement.Units)
        <|> (parse_must_statement           |>> DeviateAddBodyStatement.Must)
        <|> (parse_unique_statement         |>> DeviateAddBodyStatement.Unique)
        <|> (parse_default_statement        |>> DeviateAddBodyStatement.Default)
        <|> (parse_config_statement         |>> DeviateAddBodyStatement.Config)
        <|> (parse_mandatory_statement      |>> DeviateAddBodyStatement.Mandatory)
        <|> (parse_min_elements_statement   |>> DeviateAddBodyStatement.MinElements)
        <|> (parse_max_elements_statement   |>> DeviateAddBodyStatement.MaxElements)
        <|> (parse_unknown_statement        |>> DeviateAddBodyStatement.Unknown)

    let parse_deviate_add_statement<'a> : Parser<DeviateAddStatement, 'a> =
        // [RFC 7950, p. 201]
        //deviate-add-stmt    = deviate-keyword sep add-keyword-str optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            [units-stmt]
        //                            *must-stmt
        //                            *unique-stmt
        //                            *default-stmt
        //                            [config-stmt]
        //                            [mandatory-stmt]
        //                            [min-elements-stmt]
        //                            [max-elements-stmt]
        //                        "}") stmtsep
        // TODO: Check and enforce cardinality constraints for deviate-add-stmt
        parse_deviate_generic_statement "add" parse_deviate_add_body_statement

    let parse_deviate_delete_body_statement<'a> : Parser<DeviateDeleteBodyStatement, 'a> =
            (parse_units_statement          |>> DeviateDeleteBodyStatement.Units)
        <|> (parse_must_statement           |>> DeviateDeleteBodyStatement.Must)
        <|> (parse_unique_statement         |>> DeviateDeleteBodyStatement.Unique)
        <|> (parse_default_statement        |>> DeviateDeleteBodyStatement.Default)
        <|> (parse_unknown_statement        |>> DeviateDeleteBodyStatement.Unknown)

    let parse_deviate_delete_statement<'a> : Parser<DeviateDeleteStatement, 'a> =
        // [RFC 7950, p. 201]
        //deviate-delete-stmt = deviate-keyword sep delete-keyword-str optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            [units-stmt]
        //                            *must-stmt
        //                            *unique-stmt
        //                            *default-stmt
        //                        "}") stmtsep
        // TODO: Check and enforce cardinality constraints for deviate-delete-stmt
        parse_deviate_generic_statement "delete" parse_deviate_delete_body_statement

    let parse_deviate_replace_body_statement<'a> : Parser<DeviateReplaceBodyStatement, 'a> =
            (Types.parse_type_statement     |>> DeviateReplaceBodyStatement.Type)
        <|> (parse_units_statement          |>> DeviateReplaceBodyStatement.Units)
        <|> (parse_default_statement        |>> DeviateReplaceBodyStatement.Default)
        <|> (parse_config_statement         |>> DeviateReplaceBodyStatement.Config)
        <|> (parse_mandatory_statement      |>> DeviateReplaceBodyStatement.Mandatory)
        <|> (parse_min_elements_statement   |>> DeviateReplaceBodyStatement.MinElements)
        <|> (parse_max_elements_statement   |>> DeviateReplaceBodyStatement.MaxElements)
        <|> (parse_unknown_statement        |>> DeviateReplaceBodyStatement.Unknown)

    let parse_deviate_replace_statement<'a> : Parser<DeviateReplaceStatement, 'a> =
        // [RFC 7950, p. 202]
        //deviate-replace-stmt = deviate-keyword sep replace-keyword-str optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            [type-stmt]
        //                            [units-stmt]
        //                            [default-stmt]
        //                            [config-stmt]
        //                            [mandatory-stmt]
        //                            [min-elements-stmt]
        //                            [max-elements-stmt]
        //                        "}") stmtsep
        // TODO: Check and enforce cardinality constraints for deviate-replace-stmt
        parse_deviate_generic_statement "replace" parse_deviate_replace_body_statement

    let parse_deviate_not_supported_statement<'a> : Parser<DeviateNotSupportedStatement, 'a> =
        skipString "deviate" >>. spaces >>.
        skipString "not-supported" >>. spaces >>.
        (
                (end_of_statement                           |>> (fun _ -> None))
            <|> (begin_block >>. (block parse_statement)    |>> Some)
        )

    let parse_deviation_body_statement<'a> : Parser<DeviationBodyStatement, 'a> =
            (parse_description_statement        |>> DeviationBodyStatement.Description)
        <|> (parse_reference_statement          |>> DeviationBodyStatement.Reference)
        <|> (skipString "deviate" >>. spaces >>.
                ( (skipString "add" >>. spaces >>.
                    (       (end_of_statement                                                   |>> (fun _ -> None))
                        <|> (begin_block >>. (block_generic parse_deviate_add_body_statement)   |>> Some)
                    )
                   |>> DeviationBodyStatement.DeviateAdd)
                )
            <|> ( (skipString "delete" >>. spaces >>.
                    (       (end_of_statement                                                   |>> (fun _ -> None))
                        <|> (begin_block >>. (block_generic parse_deviate_delete_body_statement)   |>> Some)
                    )
                   |>> DeviationBodyStatement.DeviateDelete)
                )
            <|> ( (skipString "replace" >>. spaces >>.
                    (       (end_of_statement                                                   |>> (fun _ -> None))
                        <|> (begin_block >>. (block_generic parse_deviate_replace_body_statement)   |>> Some)
                    )
                   |>> DeviationBodyStatement.DeviateReplace)
                )
            <|> ( (skipString "not-supported" >>. spaces >>.
                    (       (end_of_statement                                                   |>> (fun _ -> None))
                        <|> (begin_block >>. (block_generic parse_statement)   |>> Some)
                    )
                   |>> DeviationBodyStatement.DeviateNotSupported)
                )
            )
        <|> (parse_unknown_statement        |>> DeviationBodyStatement.Unknown)

    let parse_deviation_statement<'a> : Parser<DeviationStatement, 'a> =
        // [RFC 7950, p. 201]
        //deviation-stmt      = deviation-keyword sep
        //                        deviation-arg-str optsep
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            [description-stmt]
        //                            [reference-stmt]
        //                            (deviate-not-supported-stmt /
        //                            1*(deviate-add-stmt /
        //                                deviate-replace-stmt /
        //                                deviate-delete-stmt))
        //                        "}" stmtsep
        make_statement_parser_generic "deviation" Identifier.parse_schema_node_identifier_absolute parse_deviation_body_statement
