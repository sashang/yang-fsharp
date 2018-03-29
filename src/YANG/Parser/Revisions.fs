// Revisions.fs
// Parsing and handling revisions

namespace Yang.Parser

/// Parsing and processing module revision information
[<AutoOpen>]
module Revisions =
    open FParsec
    open Yang.Model

    let parse_revision_body_statement<'a> : Parser<RevisionBodyStatement, 'a> =
            (parse_description_statement        |>> RevisionBodyStatement.Description)
        <|> (parse_reference_statement          |>> RevisionBodyStatement.Reference)
        <|> (parse_unknown_statement            |>> RevisionBodyStatement.Unknown)

    let parse_revision_statement<'a> : Parser<RevisionStatement, 'a> =
        // [RFC 7950, p. 186]
        //revision-stmt       = revision-keyword sep revision-date optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            [description-stmt]
        //                            [reference-stmt]
        //                        "}") stmtsep
        make_statement_parser_optional_generic "revision" parse_date parse_revision_body_statement
        |>> RevisionStatement

    /// Parses all revision statements
    let parse_revision_list<'a> : Parser<RevisionStatement list, 'a> =
        many parse_revision_statement
