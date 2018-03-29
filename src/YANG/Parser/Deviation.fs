// Deviation.fs
namespace Yang.Parser

/// Parser for deviation-stmt and deviate-[add|delete|replace]-stmt
[<AutoOpen>]
module Deviation =
    open FParsec
    open NLog
    open Yang.Model
    open Expressions

    //
    // Parsing of deviation statements.
    // Parsing differs slightly than the rest of the statements, because the deviate-[add|delete|replace]-stmt use the same keyword
    //

    /// Logger for this module
    let private _logger = LogManager.GetCurrentClassLogger()

    let private throw fmt =
        let do_throw (message : string) =
            _logger.Error message
            raise (YangParserException message)
        Printf.ksprintf do_throw fmt

    let private warn fmt = Printf.ksprintf _logger.Warn fmt
    let private debug fmt = Printf.ksprintf _logger.Debug fmt
    let private trace fmt = Printf.ksprintf _logger.Trace fmt
    let private error fmt = Printf.ksprintf _logger.Error fmt

#if INTERACTIVE
    // The following are used only in interactive (fsi) to help with enabling disabling
    // logging for particular modules.

    type internal Marker = interface end
    let _full_name = typeof<Marker>.DeclaringType.FullName
    let _name = typeof<Marker>.DeclaringType.Name
#endif

    let private parse_deviate_generic_statement<'a, 'b>
        (keyword    : string)
        (body       : Parser<'b, 'a>) : Parser<('b list option), 'a>
        =
            attempt (skipString "deviate" >>. spaces >>. (parse_skipString keyword) >>.
                     (spaces1 <|> (followedBy (skipChar ';')))) >>.
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
        |>> DeviateAddStatement

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
        |>> DeviateDeleteStatement

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
        |>> DeviateReplaceStatement

    let parse_deviate_not_supported_statement<'a> : Parser<DeviateNotSupportedStatement, 'a> =
        // [RFC 7950, p. 201]
       //deviate-not-supported-stmt =
       //                      deviate-keyword sep
       //                      not-supported-keyword-str stmtend
       parse_deviate_generic_statement "not-supported" parse_statement
       |>> DeviateNotSupportedStatement

    let parse_deviation_body_statement<'a> : Parser<DeviationBodyStatement, 'a> =
            (parse_description_statement            |>> DeviationBodyStatement.Description)
        <|> (parse_reference_statement              |>> DeviationBodyStatement.Reference)
        <|> (parse_deviate_add_statement            |>> DeviationBodyStatement.DeviateAdd)
        <|> (parse_deviate_replace_statement        |>> DeviationBodyStatement.DeviateReplace)
        <|> (parse_deviate_delete_statement         |>> DeviationBodyStatement.DeviateDelete)
        <|> (parse_deviate_not_supported_statement  |>> DeviationBodyStatement.DeviateNotSupported)
        <|> (parse_unknown_statement                |>> DeviationBodyStatement.Unknown)

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
        let transform_invalid_absolute_path (input : string) =
            if input.StartsWith "/" then input
            elif input.Contains(":/") then
                // HACK: The argument to the deviation-stmt does not start with '/', but maybe we need to transform it to make it valid
                //       This issue exists in some models, e.g. from Cisco Models-External\YangModels\vendor\cisco\nx\7.0-3-I7-3\cisco-nx-openconfig-bgp-deviations.yang
                //       It seems to be invalid syntax according to the spec, but the intensions are rather clear.
                warn "Detected invalid input in deviation-arg; will attempt to correct and try again"
                sprintf "/%s" (input.Replace(":/", ":"))
            else
                warn "Detected invalid input in deviation-arg; will attempt to correct and try again"
                sprintf "/%s" input

        make_statement_parser_generic "deviation" (
            pipt Strings.parse_string transform_invalid_absolute_path Identifier.parse_schema_node_identifier_absolute
        ) parse_deviation_body_statement
        |>> fun (id, body) -> DeviationStatement (Arguments.Deviation id, body)
