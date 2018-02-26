// Type.fs
// Definitions and parsing for YANG defined types

namespace Yang.Parser

// TODO: Make the Type parser part of yang-stmt definition
//       We will need a way to make the statements and parsers below known
//       in the definition of parse_statement.

/// Type definitions and parsers for types used in YANG
module Types =
    open FParsec

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

    type Type =
    | TString

    type TypeStatement = Type * ExtraStatements

    let parse_type<'a> : Parser<TypeStatement, 'a> =
        skipString "type" >>. spaces >>.
        skipString "string" >>. spaces >>.
        skipChar ';' >>. spaces
        |>> (fun _ -> TString, None)
