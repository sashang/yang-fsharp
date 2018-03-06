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
            // TODO: fill in missing parsing for ContainerBodyStatement
                (parse_config_statement         |>> ContainerBodyStatement.Config)
            <|> (parse_description_statement    |>> ContainerBodyStatement.Description)
            <|> (parse_data_definition          |>> ContainerBodyStatement.FromDataDefinition)

        let parse_container_statement : Parser<ContainerStatement, 'a> =
            // [RFC 7950, p.193]
            //container-stmt      = container-keyword sep identifier-arg-str optsep
            //                      (";" /
            //                       "{" stmtsep
            //                           ;; these stmts can appear in any order
            //                           [when-stmt]
            //                           *if-feature-stmt
            //                           *must-stmt
            //                           [presence-stmt]
            //                           [config-stmt]
            //                           [status-stmt]
            //                           [description-stmt]
            //                           [reference-stmt]
            //                           *(typedef-stmt / grouping-stmt)
            //                           *data-def-stmt
            //                           *action-stmt
            //                           *notification-stmt
            //                       "}") stmtsep

            skipString "container" >>. spaces >>.
            Identifier.parse_identifier .>> spaces .>>.
            (
                    (skipChar ';' |>> (fun _ -> None))
                <|> (skipChar '{' >>. spaces >>.
                     (many parse_container_body_statement) .>> spaces .>>
                     skipChar '}'
                     |>> Some
                    ) .>> spaces
            )

        let parse_typedef_body_statement : Parser<TypeDefBodyStatement, 'a> =
            // TODO: fill in missing parsing for TypeDefBodyStatement
                (Types.parse_type               |>> TypeDefBodyStatement.Type)
            <|> (parse_description_statement    |>> TypeDefBodyStatement.Description)
            <|> (parse_reference_statement      |>> TypeDefBodyStatement.Reference)

        let parse_typedef_statement : Parser<TypeDefStatement, 'a> =
            // [RFC 7950, p.188]
            //typedef-stmt        = typedef-keyword sep identifier-arg-str optsep
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            type-stmt
            //                            [units-stmt]
            //                            [default-stmt]
            //                            [status-stmt]
            //                            [description-stmt]
            //                            [reference-stmt]
            //                        "}" stmtsep
            skipString "typedef" >>. spaces >>.
            Identifier.parse_identifier .>> spaces .>>
            skipChar '{' .>> spaces .>>.
            (many parse_typedef_body_statement) .>> spaces .>>
            skipChar '}' .>> spaces

        let parse_list_body_statement : Parser<ListBodyStatement, 'a> =
            // TODO: fill in missing parsing for ListBodyStatement
                (skipString "key" >>. spaces >>. Strings.parse_string .>> spaces .>>. (end_of_statement_or_block parse_statement)
                 |>> (fun (key, block) -> (Arguments.Key.MakeFromString key, block) |> ListBodyStatement.Key ))
            <|> (parse_ordered_by_statement     |>> ListBodyStatement.OrderedBy)
            <|> (parse_description_statement    |>> ListBodyStatement.Description)
            <|> (parse_data_definition          |>> ListBodyStatement.FromDataDefinition)

        let parse_list_statement : Parser<ListStatement, 'a> =
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

            skipString "list" >>. spaces >>.
            Identifier.parse_identifier .>> spaces .>>
            skipChar '{' .>> spaces .>>.
            (many parse_list_body_statement) .>> spaces
            .>> skipChar '}' .>> spaces

        let parse_uses_body_statement : Parser<UsesBodyStatement, 'a> =
            // TODO: fill in missing parsing for UsesBodyStatement
                (parse_description_statement    |>> UsesBodyStatement.Description)
            <|> (parse_reference_statement      |>> UsesBodyStatement.Reference)

        let parse_uses_statement : Parser<UsesStatement, 'a> =
            // [RFC 7950, p. 197]
            //uses-stmt           = uses-keyword sep identifier-ref-arg-str optsep
            //                        (";" /
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            [when-stmt]
            //                            *if-feature-stmt
            //                            [status-stmt]
            //                            [description-stmt]
            //                            [reference-stmt]
            //                            *refine-stmt
            //                            *uses-augment-stmt
            //                        "}") stmtsep
            skipString "uses" >>. spaces >>.
            Identifier.parse_identifier_reference .>> spaces .>>.
            (
                    (skipChar ';' |>> (fun _ -> None))
                <|> (skipChar '{' >>. spaces >>.
                     (many parse_uses_body_statement) .>> spaces .>>
                     skipChar '}'
                     |>> Some
                    )
            ) .>> spaces

        let parse_data_definition_implementation : Parser<BodyStatement, 'a> =
                (parse_container_statement  |>> BodyStatement.Container)
            <|> (parse_typedef_statement    |>> BodyStatement.TypeDef)
            <|> (parse_leaf_list            |>> BodyStatement.LeafList)
            <|> (parse_leaf                 |>> BodyStatement.Leaf)
            <|> (parse_list_statement       |>> BodyStatement.List)
            <|> (parse_uses_statement       |>> BodyStatement.Uses)

        parse_data_definition_ref := parse_data_definition_implementation

        {
            DataDefinition  = parse_data_definition
            ContainerBody   = parse_container_body_statement
            Container       = parse_container_statement
        }

    let parse_body_statement<'a> : Parser<BodyStatement, 'a> = parsers.DataDefinition
    let parse_body_statements<'a> : Parser<BodyStatement list, 'a> = many parsers.DataDefinition

    let parse_container_body_statement<'a> : Parser<ContainerBodyStatement, 'a> = parsers.ContainerBody
    let parse_container_statement<'a> : Parser<ContainerStatement, 'a> = parsers.Container
