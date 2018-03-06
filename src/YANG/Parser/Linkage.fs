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
            ((parse_prefix_statement        .>> spaces) |>> ImportBodyStatement.Prefix)
        <|> ((parse_revision_date_statement .>> spaces) |>> ImportBodyStatement.RevisionDate)
        <|> ((parse_description_statement   .>> spaces) |>> ImportBodyStatement.Description)
        <|> ((parse_reference_statement     .>> spaces) |>> ImportBodyStatement.Reference)
        <|> ((parse_unknown_statement       .>> spaces) |>> ImportBodyStatement.Unknown)

    let parse_import_statement<'a> : Parser<ImportStatement, 'a> =
        skipString "import" >>. spaces >>.
        Identifier.parse_identifier .>> spaces .>>
        skipChar '{' .>> spaces .>>.
        (many parse_import_body_statement) .>>
        skipChar '}' .>> spaces

    let parse_include_body_statement<'a> : Parser<IncludeBodyStatement, 'a> =
            ((parse_revision_date_statement .>> spaces) |>> IncludeBodyStatement.RevisionDate)
        <|> ((parse_description_statement   .>> spaces) |>> IncludeBodyStatement.Description)
        <|> ((parse_reference_statement     .>> spaces) |>> IncludeBodyStatement.Reference)
        <|> ((parse_unknown_statement       .>> spaces) |>> IncludeBodyStatement.Unknown)

    let parse_include_statement<'a> : Parser<IncludeStatement, 'a> =
        skipString "include" >>. spaces >>.
        Identifier.parse_identifier .>> spaces .>>.
        (       ( skipChar ';' .>> spaces |>> (fun _ -> Option.None) )
         <|>    ( skipChar '{' .>> spaces >>.
                  (many parse_include_body_statement) .>>
                  skipChar '}' .>> spaces
                  |>> Option.Some)
        )

    let parse_linkage_statement<'a> : Parser<LinkageBodyStatement, 'a> =
            ((parse_import_statement .>> spaces)    |>> LinkageBodyStatement.Import)
        <|> ((parse_include_statement .>> spaces)   |>> LinkageBodyStatement.Include)

    let parse_linkage_section<'a> : Parser<LinkageStatements, 'a> = many parse_linkage_statement
