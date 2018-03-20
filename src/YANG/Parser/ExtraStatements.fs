// ExtraStatements.fs
namespace Yang.Parser

/// Parsers for common statements, that appear in the yang-stmt rule.
// Due to dependencies, they appear in this file.
[<AutoOpen>]
module ExtraStatements =
    open FParsec
    open Yang.Model
    open Expressions

    let parse_feature_body_statement<'a> : Parser<FeatureBodyStatement, 'a> =
            (parse_if_feature_statement     |>> FeatureBodyStatement.IfFeature)
        <|> (parse_status_statement         |>> FeatureBodyStatement.Status)
        <|> (parse_description_statement    |>> FeatureBodyStatement.Description)
        <|> (parse_reference_statement      |>> FeatureBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> FeatureBodyStatement.Unknown)

    let parse_feature_statement<'a> : Parser<FeatureStatement, 'a> =
        // [RFC 7950, p. 187]
        //feature-stmt        = feature-keyword sep identifier-arg-str optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            *if-feature-stmt
        //                            [status-stmt]
        //                            [description-stmt]
        //                            [reference-stmt]
        //                        "}") stmtsep
        // TODO: Check and enforce cardinality of feature-stmt
        make_statement_parser_optional_generic "feature" Identifier.parse_identifier parse_feature_body_statement

    let parse_identity_body_statement<'a> : Parser<IdentityBodyStatement, 'a> =
            (parse_if_feature_statement     |>> IdentityBodyStatement.IfFeature)
        <|> (parse_base_statement           |>> IdentityBodyStatement.Base)
        <|> (parse_status_statement         |>> IdentityBodyStatement.Status)
        <|> (parse_description_statement    |>> IdentityBodyStatement.Description)
        <|> (parse_reference_statement      |>> IdentityBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> IdentityBodyStatement.Unknown)

    let parse_identity_statement<'a> : Parser<IdentityStatement, 'a> =
        // [RFC 7950, p. 187]
        //identity-stmt       = identity-keyword sep identifier-arg-str optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            *if-feature-stmt
        //                            *base-stmt
        //                            [status-stmt]
        //                            [description-stmt]
        //                            [reference-stmt]
        //                        "}") stmtsep
        // TODO: Check and enforce cardinality of refine-stmt body
        make_statement_parser_optional_generic "refine" Identifier.parse_identifier parse_identity_body_statement

    let parse_enum_body_statement<'a> : Parser<EnumBodyStatement, 'a> =
        // TODO: Fill in the rest cases for EnumBodyStatement
            (parse_if_feature_statement     |>> EnumBodyStatement.IfFeature)
        <|> (parse_value_statement          |>> EnumBodyStatement.Value)
        <|> (parse_status_statement         |>> EnumBodyStatement.Status)
        <|> (parse_description_statement    |>> EnumBodyStatement.Description)
        <|> (parse_reference_statement      |>> EnumBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> EnumBodyStatement.Unknown)

    let parse_enum_statement<'a> : Parser<EnumStatement, 'a> =
        // [RFC 7950, p. 190]
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
        make_statement_parser_optional_generic "enum" Strings.parse_string parse_enum_body_statement

    let parse_refine_body_statement<'a> : Parser<RefineBodyStatement, 'a> =
            (parse_if_feature_statement |>> RefineBodyStatement.IfFeature)
        <|> (parse_default_statement    |>> RefineBodyStatement.Default)
        <|> (parse_config_statement     |>> RefineBodyStatement.Config)
        <|> (parse_mandatory_statement  |>> RefineBodyStatement.Mandatory)
        <|> (parse_min_elemenets_statement  |>> RefineBodyStatement.MinElements)
        <|> (parse_max_elements_statement   |>> RefineBodyStatement.MaxElements)
        <|> (parse_description_statement    |>> RefineBodyStatement.Description)
        <|> (parse_reference_statement  |>> RefineBodyStatement.Reference)
        <|> (parse_unknown_statement    |>> RefineBodyStatement.Unknown)

    /// Parses a range stateement
    let parse_refine_statement<'a> : Parser<RefineStatement, 'a> =
        // [RFC 7950, p. 189]
        //refine-stmt         = refine-keyword sep refine-arg-str optsep
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            *if-feature-stmt
        //                            *must-stmt
        //                            [presence-stmt]
        //                            *default-stmt
        //                            [config-stmt]
        //                            [mandatory-stmt]
        //                            [min-elements-stmt]
        //                            [max-elements-stmt]
        //                            [description-stmt]
        //                            [reference-stmt]
        //                        "}" stmtsep
        make_statement_parser_optional_generic "refine" Identifier.parse_schema_node_identifier_descendant parse_refine_body_statement
