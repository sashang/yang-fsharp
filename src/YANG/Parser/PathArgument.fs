// PathArgument.fs
// Parsing of path arguments

namespace Yang.Parser

[<AutoOpen>]
module PathArgument =
    open FParsec
    open Yang.Model.Arguments

    // [RFC 7950, p. 205-206]
    //path-arg            = absolute-path / relative-path
    //absolute-path       = 1*("/" (node-identifier *path-predicate))
    //relative-path       = 1*("../") descendant-path
    //descendant-path     = node-identifier
    //                        [*path-predicate absolute-path]
    //path-predicate      = "[" *WSP path-equality-expr *WSP "]"
    //path-equality-expr  = node-identifier *WSP "=" *WSP path-key-expr
    //path-key-expr       = current-function-invocation *WSP "/" *WSP
    //                        rel-path-keyexpr
    //rel-path-keyexpr    = 1*(".." *WSP "/" *WSP)
    //                        *(node-identifier *WSP "/" *WSP)
    //                        node-identifier
    // [RFC 7950, p.208]
    // current-function-invocation = current-keyword *WSP "(" *WSP ")"

    let parse_path_predicate<'a> : Parser<PathPredicate, 'a> =
        skipChar '[' >>. spaces >>.
        Identifier.parse_identifier_reference .>> spaces .>>
        skipChar '=' .>> spaces .>>
        skipString "current" .>> spaces .>>
        skipChar '(' .>> spaces .>> skipChar ')' .>> spaces .>>
        skipChar '/' .>> spaces .>>.
        (many1 (pstring ".." .>> spaces .>> skipChar '/' .>> spaces)) .>>.
        (sepBy1 Identifier.parse_identifier_reference (spaces .>> skipChar '/' .>> spaces))
        .>> spaces .>> skipChar ']'
        |>> (
            fun ((id, ups), rel) ->
                let key = PathKey (uint16 ups.Length, rel)
                PathPredicate (id, key)
        )

    let parse_path_item<'a> : Parser<PathItem, 'a> =
        Identifier.parse_identifier_reference .>>. (opt (many parse_path_predicate))
        |>> PathItem

    let parse_path_absolute<'a> : Parser<AbsolutePath, 'a> =
        skipChar '/' >>. (sepBy1 parse_path_item (skipChar '/')) |>> AbsolutePath

    let parse_path_relative<'a> : Parser<RelativePath, 'a> =
        many1 (pstring "../") .>>. (sepBy1 parse_path_item (skipChar '/'))
        |>> (
            fun (upwards, descendant) ->
                RelativePath (uint16 upwards.Length, descendant)
        )

    let parse_path<'a> : Parser<Path, 'a> =
            (parse_path_absolute    |>> Path.Absolute)
        <|> (parse_path_relative    |>> Path.Relative)
