// ExtraStatements.fs
namespace Yang.Parser

/// Parsers for common statements, that appear in the yang-stmt rule.
// Due to dependencies, they appear in this file.
[<AutoOpen>]
module ExtraStatements =
    open FParsec
    open Yang.Model
    open Expressions

    let parse_any_data_body_statement<'a> : Parser<AnyDataBodyStatement, 'a> =
            (parse_when_statement           |>> AnyDataBodyStatement.When)
        <|> (parse_if_feature_statement     |>> AnyDataBodyStatement.IfFeature)
        <|> (parse_must_statement           |>> AnyDataBodyStatement.Must)
        <|> (parse_config_statement         |>> AnyDataBodyStatement.Config)
        <|> (parse_mandatory_statement      |>> AnyDataBodyStatement.Mandatory)
        <|> (parse_status_statement         |>> AnyDataBodyStatement.Status)
        <|> (parse_description_statement    |>> AnyDataBodyStatement.Description)
        <|> (parse_reference_statement      |>> AnyDataBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> AnyDataBodyStatement.Unknown)

    let parse_any_data_statement<'a> : Parser<AnyDataStatement, 'a> =
        // [RFC7950, p. 197]
        //anydata-stmt        = anydata-keyword sep identifier-arg-str optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            [when-stmt]
        //                            *if-feature-stmt
        //                            *must-stmt
        //                            [config-stmt]
        //                            [mandatory-stmt]
        //                            [status-stmt]
        //                            [description-stmt]
        //                            [reference-stmt]
        //                        "}") stmtsep
        make_statement_parser_optional_generic "anydata" Identifier.parse_identifier parse_any_data_body_statement
        |>> AnyDataStatement

    let parse_any_xml_body_statement<'a> : Parser<AnyXmlBodyStatement, 'a> =
            (parse_when_statement           |>> AnyXmlBodyStatement.When)
        <|> (parse_if_feature_statement     |>> AnyXmlBodyStatement.IfFeature)
        <|> (parse_must_statement           |>> AnyXmlBodyStatement.Must)
        <|> (parse_config_statement         |>> AnyXmlBodyStatement.Config)
        <|> (parse_mandatory_statement      |>> AnyXmlBodyStatement.Mandatory)
        <|> (parse_status_statement         |>> AnyXmlBodyStatement.Status)
        <|> (parse_description_statement    |>> AnyXmlBodyStatement.Description)
        <|> (parse_reference_statement      |>> AnyXmlBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> AnyXmlBodyStatement.Unknown)

    let parse_any_xml_statement<'a> : Parser<AnyXmlStatement, 'a> =
        // [RFC7950, p. 197]
        //anyxml-stmt         = anyxml-keyword sep identifier-arg-str optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            [when-stmt]
        //                            *if-feature-stmt
        //                            *must-stmt
        //                            [config-stmt]
        //                            [mandatory-stmt]
        //                            [status-stmt]
        //                            [description-stmt]
        //                            [reference-stmt]
        //                        "}") stmtsep
        make_statement_parser_optional_generic "anyxml" Identifier.parse_identifier parse_any_xml_body_statement
        |>> AnyXmlStatement

    let parse_bit_body_statement<'a> : Parser<BitBodyStatement, 'a> =
            (parse_if_feature_statement     |>> BitBodyStatement.IfFeature)
        <|> (parse_position_statement       |>> BitBodyStatement.Position)
        <|> (parse_status_statement         |>> BitBodyStatement.Status)
        <|> (parse_description_statement    |>> BitBodyStatement.Description)
        <|> (parse_reference_statement      |>> BitBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> BitBodyStatement.Unknown)

    let parse_bit_statement<'a> : Parser<BitStatement, 'a> =
        // [RFC7950, p. 191]
        //bit-stmt            = bit-keyword sep identifier-arg-str optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            *if-feature-stmt
        //                            [position-stmt]
        //                            [status-stmt]
        //                            [description-stmt]
        //                            [reference-stmt]
        //                        "}") stmtsep
        make_statement_parser_optional_generic "bit" Identifier.parse_identifier parse_bit_body_statement
        |>> BitStatement

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
        |>> FeatureStatement

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
        make_statement_parser_optional_generic "identity" Identifier.parse_identifier parse_identity_body_statement
        |>> IdentityStatement

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
        |>> EnumStatement

    let parse_refine_body_statement<'a> : Parser<RefineBodyStatement, 'a> =
            (parse_if_feature_statement |>> RefineBodyStatement.IfFeature)
        <|> (parse_must_statement       |>> RefineBodyStatement.Must)
        <|> (parse_presence_statement   |>> RefineBodyStatement.Presence)
        <|> (parse_default_statement    |>> RefineBodyStatement.Default)
        <|> (parse_config_statement     |>> RefineBodyStatement.Config)
        <|> (parse_mandatory_statement  |>> RefineBodyStatement.Mandatory)
        <|> (parse_min_elements_statement   |>> RefineBodyStatement.MinElements)
        <|> (parse_max_elements_statement   |>> RefineBodyStatement.MaxElements)
        <|> (parse_description_statement    |>> RefineBodyStatement.Description)
        <|> (parse_reference_statement  |>> RefineBodyStatement.Reference)
        <|> (parse_unknown_statement    |>> RefineBodyStatement.Unknown)

    /// Parses a range statement
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
        // TODO: Check and enforce cardinality for refine-stmt
        make_statement_parser_optional_generic "refine" Identifier.parse_schema_node_identifier_descendant parse_refine_body_statement
        |>> RefineStatement
