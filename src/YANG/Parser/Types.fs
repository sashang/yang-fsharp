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
        let elementParser =
                (parse_length_statement  .>> spaces |>> StringRestriction.Length)
            <|> (parse_pattern_statement .>> spaces |>> StringRestriction.Pattern)

        let stateFromFirstElement = function
        | StringRestriction.Length  length  -> Some length, []
        | StringRestriction.Pattern pattern -> None, [pattern]

        let foldState state element =
            match state, element with
            | (None, patterns), StringRestriction.Length length -> Some length, patterns
            | (Some length, _), StringRestriction.Length _      ->
                let msg = sprintf "Error in parsing string restrictions: found duplicate definition of length"
                _logger.Error msg
                raise (YangParserException msg)

            | (length, patterns), StringRestriction.Pattern pattern  -> length, patterns @ [pattern]

        ParserHelper.ConsumeMany(
            elementParser           = elementParser,
            stateFromFirstElement   = stateFromFirstElement,
            foldState               = foldState,
            resultFromState         = id,
            resultForEmptySequence  = fun _ -> None, []
        )

    let parse_type_body_statement<'a> : Parser<TypeBodyStatement, 'a> =
            (parse_type_body_string_restrictions    |>> TypeBodyStatement.StringRestrictions)

    let parse_type_statement<'a> : Parser<TypeStatement, 'a> =
        // TODO: check the type parsing below for the case of a body with no unknown statements.

        skipString "type" >>. spaces >>.
        Identifier.parse_identifier_reference .>> spaces .>>.
        (       skipChar ';'                    |>> (fun _ -> None, None)
            <|> (skipChar '{' >>. spaces >>.
                 (opt (many parse_unknown_statement)) .>> spaces .>>.
                 (opt parse_type_body_statement) .>> spaces .>>
                 skipChar '}'
                 |>> (fun (unknowns, restrictions) -> restrictions, unknowns)
                )
        ) .>> spaces
        |>> (fun (id, (restrictions, unknown)) -> id, restrictions, unknown)
