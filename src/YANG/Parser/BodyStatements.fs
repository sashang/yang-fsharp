// DataDefinitions.fs
// Definitions and parsing for YANG defined types

namespace Yang.Parser

module BodyStatements =
    open FParsec
    open Yang.Model
    open Leaf
    open LeafList

    // [RFC 7950, p. 185]
    //body-stmts          = *(extension-stmt /
    //                        feature-stmt /
    //                        identity-stmt /
    //                        typedef-stmt /
    //                        grouping-stmt /
    //                        data-def-stmt /
    //                        augment-stmt /
    //                        rpc-stmt /
    //                        notification-stmt /
    //                        deviation-stmt)
    //
    // [RFC 7950, p.185]
    //data-def-stmt       = container-stmt /
    //                        leaf-stmt /
    //                        leaf-list-stmt /
    //                        list-stmt /
    //                        choice-stmt /
    //                        anydata-stmt /
    //                        anyxml-stmt /
    //                        uses-stmt
    //
    // [RFC 7950, p. 193]
    //container-stmt      = container-keyword sep identifier-arg-str optsep
    //                        (";" /
    //                        "{" stmtsep
    //                            ;; these stmts can appear in any order
    //                            [when-stmt]
    //                            *if-feature-stmt
    //                            *must-stmt
    //                            [presence-stmt]
    //                            [config-stmt]
    //                            [status-stmt]
    //                            [description-stmt]
    //                            [reference-stmt]
    //                            *(typedef-stmt / grouping-stmt)
    //                            *data-def-stmt
    //                            *action-stmt
    //                            *notification-stmt
    //                        "}") stmtsep
    //
    // [RFC 7950, p. 195]
    //list-stmt           = list-keyword sep identifier-arg-str optsep
    //                        "{" stmtsep
    //                            ;; these stmts can appear in any order
    //                            [when-stmt]
    //                            *if-feature-stmt
    //                            *must-stmt
    //                            [key-stmt]
    //                            *unique-stmt
    //                            [config-stmt]
    //                            [min-elements-stmt]
    //                            [max-elements-stmt]
    //                            [ordered-by-stmt]
    //                            [status-stmt]
    //                            [description-stmt]
    //                            [reference-stmt]
    //                            *(typedef-stmt / grouping-stmt)
    //                            1*data-def-stmt
    //                            *action-stmt
    //                            *notification-stmt
    //                        "}" stmtsep
    //key-stmt            = key-keyword sep key-arg-str stmtend
    //key-arg-str         = < a string that matches the rule >
    //                        < key-arg >
    //key-arg             = node-identifier *(sep node-identifier)

    // TODO: Parsers for list keys

    type private BodyParsers<'a> = {
        DataDefinition  : Parser<BodyStatement, 'a>
        ContainerBody   : Parser<ContainerBodyStatement, 'a>
        Container       : Parser<ContainerStatement, 'a>
    }

    let private parsers<'a> =
        let (parse_data_definition : Parser<BodyStatement, 'a>), (parse_data_definition_ref : Parser<BodyStatement, 'a> ref) =
            createParserForwardedToRef<BodyStatement, 'a>()

        let parse_container_body_statement : Parser<ContainerBodyStatement, 'a> =
                (parse_description_statement    |>> ContainerBodyStatement.Description)
            <|> (parse_data_definition          |>> ContainerBodyStatement.FromDataDefinition)

        let parse_container_statement : Parser<ContainerStatement, 'a> =
            skipString "container" >>. spaces >>.
            Identifier.parse_identifier .>> spaces .>>.
            (
                    (skipChar ';' |>> (fun _ -> None))
                <|> (skipChar '{' >>. spaces >>.
                     (many parse_container_body_statement) .>> spaces .>>
                     skipChar '}'
                     |>> Some
                    )
            )

        let parse_data_definition_impl : Parser<BodyStatement, 'a> =
                (parse_container_statement  |>> BodyStatement.Container)
            <|> (parse_leaf                 |>> BodyStatement.Leaf)
            <|> (parse_leaf_list            |>> BodyStatement.LeafList)

        parse_data_definition_ref := parse_data_definition_impl

        {
            DataDefinition  = parse_data_definition
            ContainerBody   = parse_container_body_statement
            Container       = parse_container_statement
        }

    let parse_body_statement<'a> : Parser<BodyStatement, 'a> = parsers.DataDefinition
    let parse_body_statements<'a> : Parser<BodyStatement list, 'a> =
        many parsers.DataDefinition

    //let parse_data_definition<'a> : Parser<DataDefinitionStatement, 'a> =
    //    let (parse_data_definition : Parser<DataDefinitionStatement, 'a>),
    //        (parse_data_definition_ref : Parser<DataDefinitionStatement, 'a> ref) =
    //            createParserForwardedToRef<DataDefinitionStatement, 'a>()

    //    let parse_container_body =
    //        let parse_container_body_item =
    //            parse_data_definition |>> ContainerStatementBody.DataDefinition
    //        many parse_container_body_item

    //    let parse_container =
    //        skipString "container" >>. spaces >>.
    //        Identifier.parse_identifier .>> spaces .>>.
    //        (
    //                (skipChar ';'                                                                   |>> (fun _ -> None))
    //            <|> (skipChar '{' >>. spaces >>. parse_container_body .>> spaces .>> skipChar '}'   |>> Some)
    //        ) .>> spaces
    //        |>> (fun (name, body) -> Container (name, body))

    //    let parse_list_body =
    //        let parse_list_body_item =
    //                (skipString "key" >>. spaces >>. Strings.parse_string .>> spaces .>> skipChar ';' .>> spaces |>> YListStatementBody.ListKey)
    //            <|> (parse_data_definition |>> YListStatementBody.DataDefinition)
    //        many parse_list_body_item

    //    let parse_list =
    //        skipString "list" >>. spaces >>.
    //        Identifier.parse_identifier .>> spaces .>>
    //        skipChar '{' .>> spaces .>>.
    //        parse_list_body .>> spaces
    //        .>> skipChar '}' .>> spaces
    //        |>> (fun (name, body) -> YList (name, body))

    //    let inline parse_data_definition_implementation (input : CharStream<'a>) : Reply<DataDefinitionStatement> =
    //        let parser =
    //                parse_container
    //            <|> (LeafList.parse_leaf_list   |>> LeafList)
    //            <|> (Leaf.parse_leaf            |>> Leaf)
    //            <|> parse_list

    //        parser input

    //    parse_data_definition_ref := parse_data_definition_implementation
    //    parse_data_definition

