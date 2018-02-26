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

    let IsTypeStatement         = function | Type _ -> true         | _ -> false
    let IsDescriptionStatement  = function | Description _ -> true  | _ -> false

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


module LeafList =
    open FParsec
    open Types

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

    type LeafListStatement =
    | Type          of TypeStatement
    | Description   of DescriptionStatement
    | Unknown       of UnknownStatement

    let IsTypeStatement         = function | Type _ -> true         | _ -> false
    let IsDescriptionStatement  = function | Description _ -> true  | _ -> false

    type LeafList = {
        Identifier  : Identifier.Identifier
        Statements  : LeafListStatement list
    }
        with
            member this.Type        =
                match List.find IsTypeStatement this.Statements with
                | Type statement -> statement
                | _              -> raise (YangParserException (sprintf "Cannot find type for %s" this.Identifier.Value))

            member this.Description = List.tryFind IsDescriptionStatement this.Statements

    let private parse_leaf_body<'a> : Parser<LeafListStatement list, 'a> =
        many (
                (Types.parse_type |>> Type)
            <|> (parse_description_statement |>> Description)
        )

    let parse_leaf_list<'a> : Parser<LeafList, 'a> =
        skipString "leaf-list" >>. spaces >>.
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
