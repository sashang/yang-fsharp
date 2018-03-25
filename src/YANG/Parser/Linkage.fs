// Generic.fs
// Parsing and support for import and include statements

namespace Yang.Parser

module Linkage =
    open FParsec
    open NLog
    open Yang.Model

    // Parsing of linkage statements, [RFC 7950, p.185]
    //linkage-stmts       = ;; these stmts can appear in any order
    //                        *import-stmt
    //                        *include-stmt

    //import-stmt         = import-keyword sep identifier-arg-str optsep
    //                        "{" stmtsep
    //                            ;; these stmts can appear in any order
    //                            prefix-stmt
    //                            [revision-date-stmt]
    //                            [description-stmt]
    //                            [reference-stmt]
    //                        "}" stmtsep

    //include-stmt        = include-keyword sep identifier-arg-str optsep
    //                        (";" /
    //                        "{" stmtsep
    //                            ;; these stmts can appear in any order
    //                            [revision-date-stmt]
    //                            [description-stmt]
    //                            [reference-stmt]
    //                        "}") stmtsep

    /// Logger for this module
    let private _logger = LogManager.GetCurrentClassLogger()

#if INTERACTIVE
    // The following are used only in interactive (fsi) to help with enabling disabling
    // logging for particular modules.

    type internal Marker = interface end
    let _full_name = typeof<Marker>.DeclaringType.FullName
    let _name = typeof<Marker>.DeclaringType.Name
#endif

    let parse_import_body_statement<'a> : Parser<ImportBodyStatement, 'a> =
            (parse_prefix_statement         |>> ImportBodyStatement.Prefix)
        <|> (parse_revision_date_statement  |>> ImportBodyStatement.RevisionDate)
        <|> (parse_description_statement    |>> ImportBodyStatement.Description)
        <|> (parse_reference_statement      |>> ImportBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> ImportBodyStatement.Unknown)

    let parse_import_statement<'a> : Parser<ImportStatement, 'a> =
        make_statement_parser_generic "import" Identifier.parse_identifier parse_import_body_statement
        |>> ImportStatement

    let parse_include_body_statement<'a> : Parser<IncludeBodyStatement, 'a> =
            (parse_revision_date_statement  |>> IncludeBodyStatement.RevisionDate)
        <|> (parse_description_statement    |>> IncludeBodyStatement.Description)
        <|> (parse_reference_statement      |>> IncludeBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> IncludeBodyStatement.Unknown)

    let parse_include_statement<'a> : Parser<IncludeStatement, 'a> =
        make_statement_parser_optional_generic "include" (pip Strings.parse_string Identifier.parse_identifier) parse_include_body_statement
        |>> IncludeStatement

    let parse_linkage_statement<'a> : Parser<LinkageBodyStatement, 'a> =
            (parse_import_statement     |>> LinkageBodyStatement.Import)
        <|> (parse_include_statement    |>> LinkageBodyStatement.Include)

    let parse_linkage_section<'a> : Parser<LinkageStatements, 'a> = many parse_linkage_statement
