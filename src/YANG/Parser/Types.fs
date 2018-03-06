// Type.fs
// Definitions and parsing for YANG defined types

namespace Yang.Parser

/// Type definitions and parsers for types used in YANG
module Types =
    open FParsec
    open Yang.Model
    open System

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

    let parse_type<'a> : Parser<TypeStatement, 'a> =
        skipString "type" >>. spaces >>.
        Identifier.parse_identifier_reference .>> spaces .>>.
        (       skipChar ';'                    |>> (fun _ -> None, None)
            <|> (skipChar '{' >>. spaces >>.
                 (many parse_unknown_statement) .>> spaces .>>
                 skipChar '}'
                 |>> (fun unknowns -> None, Some unknowns)
                )
        ) .>> spaces
        |>> (fun (id, (statements, unknown)) -> id, statements, unknown)
