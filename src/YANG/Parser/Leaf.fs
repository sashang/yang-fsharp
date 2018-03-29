// Leaf.fs
// Definitions and parsing for the leaf and leaf-list statements

namespace Yang.Parser

// TODO: Repetition between Leaf and LeafList
//       Leaf and LeafList share a lot of common code.
//       Does it make sense to try to unify their implementation?

// TODO: Make Leaf and LeafList statements part of yang-stmt definition
//       We will need a way to make the statements and parsers below known
//       in the definition of parse_statement.

// TODO: Rename Leaf and LeafList to LeafStatement and LeafListStatement
//       and also LeafStatementItem and LeafListStatementItem.

/// Type definitions and parsers for the leaf and leaf-list statements
module Leaf =
    open FParsec
    open Types
    open Yang.Model

    // [RFC 7950, p. 194]
    //leaf-stmt           = leaf-keyword sep identifier-arg-str optsep
    //                        "{" stmtsep
    //                            ;; these stmts can appear in any order
    //                            [when-stmt]
    //                            *if-feature-stmt
    //                            type-stmt
    //                            [units-stmt]
    //                            *must-stmt
    //                            [default-stmt]
    //                            [config-stmt]
    //                            [mandatory-stmt]
    //                            [status-stmt]
    //                            [description-stmt]
    //                            [reference-stmt]
    //                        "}" stmtsep

    let parse_leaf_body_statement<'a> : Parser<LeafBodyStatement, 'a> =
        // TODO: Enforce the cardinality constraints for LeafBodyStatement
            (parse_when_statement           |>> LeafBodyStatement.When)
        <|> (Expressions.parse_if_feature_statement |>> LeafBodyStatement.IfFeature)
        <|> (Types.parse_type_statement     |>> LeafBodyStatement.Type)
        <|> (parse_units_statement          |>> LeafBodyStatement.Units)
        <|> (parse_must_statement           |>> LeafBodyStatement.Must)
        <|> (parse_default_statement        |>> LeafBodyStatement.Default)
        <|> (parse_config_statement         |>> LeafBodyStatement.Config)
        <|> (parse_mandatory_statement      |>> LeafBodyStatement.Mandatory)
        <|> (parse_status_statement         |>> LeafBodyStatement.Status)
        <|> (parse_description_statement    |>> LeafBodyStatement.Description)
        <|> (parse_reference_statement      |>> LeafBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> LeafBodyStatement.Unknown)

    let parse_leaf_statement<'a> : Parser<LeafStatement, 'a> =
        make_statement_parser_generic "leaf" parse_identifier parse_leaf_body_statement
        |>> LeafStatement

module LeafList =
    open FParsec
    open Types
    open Yang.Model

    // [RFC 7950, p. 194]
    //leaf-list-stmt      = leaf-list-keyword sep identifier-arg-str optsep
    //                        "{" stmtsep
    //                            ;; these stmts can appear in any order
    //                            [when-stmt]
    //                            *if-feature-stmt
    //                            type-stmt stmtsep
    //                            [units-stmt]
    //                            *must-stmt
    //                            *default-stmt
    //                            [config-stmt]
    //                            [min-elements-stmt]
    //                            [max-elements-stmt]
    //                            [ordered-by-stmt]
    //                            [status-stmt]
    //                            [description-stmt]
    //                            [reference-stmt]
    //                        "}" stmtsep

    let private parse_leaf_body_statement<'a> : Parser<LeafListBodyStatement, 'a> =
        // TODO: fill in the missing statements for LeafListBodyStatement
        // TODO: Enforce the cardinality constraints for LeafListBodyStatement
            (parse_when_statement           |>> LeafListBodyStatement.When)
        <|> (Expressions.parse_if_feature_statement |>> LeafListBodyStatement.IfFeature)
        <|> (Types.parse_type_statement     |>> LeafListBodyStatement.Type)
        <|> (parse_units_statement          |>> LeafListBodyStatement.Units)
        <|> (parse_must_statement           |>> LeafListBodyStatement.Must)
        <|> (parse_default_statement        |>> LeafListBodyStatement.Default)
        <|> (parse_config_statement         |>> LeafListBodyStatement.Config)
        <|> (parse_min_elements_statement   |>> LeafListBodyStatement.MinElements)
        <|> (parse_max_elements_statement   |>> LeafListBodyStatement.MaxElements)
        <|> (parse_ordered_by_statement     |>> LeafListBodyStatement.OrderedBy)
        <|> (parse_status_statement         |>> LeafListBodyStatement.Status)
        <|> (parse_description_statement    |>> LeafListBodyStatement.Description)
        <|> (parse_reference_statement      |>> LeafListBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> LeafListBodyStatement.Unknown)

    let parse_leaf_list_statement<'a> : Parser<LeafListStatement, 'a> =
        make_statement_parser_generic "leaf-list" Identifier.parse_identifier parse_leaf_body_statement
        |>> LeafListStatement
