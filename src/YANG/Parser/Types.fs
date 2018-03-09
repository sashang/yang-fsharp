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

    type private StringRestriction =
    | Length of LengthStatement
    | Pattern of PatternStatement

    let parse_type_body_string_restrictions<'a> : Parser<StringRestrictions, 'a> =
        // [RFC 7950, p. 189]
        //string-restrictions = ;; these stmts can appear in any order
        //                        [length-stmt]
        //                        *pattern-stmt
            (parse_length_statement     |>> fun v -> Some v, [])
        <|> (parse_pattern_statement    |>> fun v -> None,   [v])

    type private TypeBodyStatementStep =
    | Restriction   of TypeBodyStatement
    | Unknown       of UnknownStatement

    let private collect_type_body_statements (statements : TypeBodyStatementStep list) =
        _logger.Debug("Inside Type:collect_type_body_statement")
        statements
        |> List.fold (
            fun (unknowns, restriction) statement ->
                match unknowns, restriction, statement with
                | None, None, Unknown statement                     -> Some [ statement ], None
                | Some unknowns', None, Unknown statement           -> Some ( unknowns' @ [statement] ), None
                | _, None, Restriction restriction                  -> unknowns, Some restriction
                | _, Some old, Restriction restriction              ->
                    _logger.Error("Found second restriction; only one type restriction allowed.\nOld restriction: {0}\nNew restriction: {1}",
                                  old, restriction)
                    raise (YangParserException "Found multiple type restrictions; only one allowed")
                | _, Some _, Unknown _                              ->
                    _logger.Error("Found unknown statement after the first type restriction. Restriction: {0}; Statement: {1}", restriction, statement)
                    raise (YangParserException "Found unknown statement after parsing one or more type restrictions")
        ) (None, None)

    let private parse_type_body_statement<'a> : Parser<TypeBodyStatementStep, 'a> =
            (parse_type_body_string_restrictions    |>> (fun v -> Restriction (TypeBodyStatement.StringRestrictions v)))
        <|> (parse_unknown_statement                |>> (fun v -> Unknown v))

    let parse_type_statement<'a> : Parser<TypeStatement, 'a> =
        // TODO: check the type parsing below for the case of a body with no unknown statements.
        make_statement_parser_optional_generic "type" Identifier.parse_identifier_reference parse_type_body_statement
        |>> (fun (id, statements) ->
                match statements with
                | None              ->
                    _logger.Debug("Finished parsing type statement: {0}", id)
                    id, None, None
                | Some statements'  ->
                    let unknowns, restriction = collect_type_body_statements statements'
                    _logger.Debug("Finished parsing type statement: {0} with '{1}' and '{2}'", id, restriction, unknowns)
                    id, restriction, unknowns
            )
