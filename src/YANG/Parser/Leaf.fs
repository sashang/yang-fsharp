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

    type LeafStatement =
    | Type          of TypeStatement
    | Description   of DescriptionStatement
    | Unknown       of UnknownStatement

    let private IsTypeStatement = function
    | Type _ -> true
    | _      -> false

    let private IsDescriptionStatement = function
    | Description _ -> true
    | _             -> false

    type Leaf = {
        Identifier  : Identifier.Identifier
        Statements  : LeafStatement list
    }
        with
            member this.Type        =
                match List.find IsTypeStatement this.Statements with
                | Type statement -> statement
                | _              -> raise (YangParserException (sprintf "Cannot find type for %s" this.Identifier.Value))

            member this.Description = List.tryFind IsDescriptionStatement this.Statements

    let private parse_leaf_body<'a> : Parser<LeafStatement list, 'a> =
        many (
                (Types.parse_type |>> Type)
            <|> (parse_description_statement |>> Description)
        )

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
                    Statements  = body
                }
        )

