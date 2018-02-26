// Leaf.fs
// Definitions and parsing for the leaf and leaf-list statements

namespace Yang.Parser

/// Type definitions and parsers for the leaf and leaf-list statements
module Leaf =
    open FParsec
    open Types

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

    type Leaf = {
        Identifier  : Identifier.Identifier
        Type        : Types.Type
        Description : DescriptionStatement option
    }

    type private LeafBody = {
        Type        : Types.Type option
        Description : DescriptionStatement option
    }
    with
        static member Empty = {
            Type        = None
            Description = None
        }

    let private parse_leaf_body<'a> : Parser<LeafBody, 'a> =
        failwith "Not implemented exception"

    let parse_leaf<'a> : Parser<Leaf, 'a> =
        skipString "leaf" >>. spaces >>.
        Identifier.parse_identifier .>> spaces .>>
        skipChar '{' .>> spaces .>>.
        parse_leaf_body .>> spaces .>>
        skipChar '}' .>> spaces
        |>> (
            fun (name, body) ->
                {
                    Identifier  = name
                    Type        = body.Type.Value
                    Description = body.Description
                }
        )

