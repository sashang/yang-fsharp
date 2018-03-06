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
        Body            : Parser<BodyStatement, 'a>
        DataDefinition  : Parser<BodyStatement, 'a>
        ContainerBody   : Parser<ContainerBodyStatement, 'a>
        Container       : Parser<ContainerStatement, 'a>
    }

    let private parsers<'a> =
        let (parse_data_definition : Parser<BodyStatement, 'a>), (parse_data_definition_ref : Parser<BodyStatement, 'a> ref) =
            createParserForwardedToRef<BodyStatement, 'a>()

        let (parse_grouping : Parser<GroupingStatement, 'a>), (parse_grouping_ref : Parser<GroupingStatement, 'a> ref) =
            createParserForwardedToRef<GroupingStatement, 'a>()

        let (parse_choice_statement : Parser<ChoiceStatement, 'a>), (parse_choice_statement_ref : Parser<ChoiceStatement, 'a> ref) =
            createParserForwardedToRef<ChoiceStatement, 'a>()

        let parse_container_body_statement : Parser<ContainerBodyStatement, 'a> =
            // TODO: fill in missing parsing for ContainerBodyStatement
                (parse_config_statement         |>> ContainerBodyStatement.Config)
            <|> (parse_status_statement         |>> ContainerBodyStatement.Status)
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
            <|> (parse_status_statement         |>> TypeDefBodyStatement.Status)
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

        let parse_grouping_body_statement : Parser<GroupingBodyStatement, 'a> =
            // TODO: fill in missing parsing for GroupingBodyStatement
                (parse_status_statement             |>> GroupingBodyStatement.Status)
            <|> (parse_description_statement        |>> GroupingBodyStatement.Description)
            <|> (parse_reference_statement          |>> GroupingBodyStatement.Reference)
            <|> (parse_typedef_statement            |>> GroupingBodyStatement.TypeDef)
            <|> (parse_grouping                     |>> GroupingBodyStatement.Grouping)
            <|> (parse_data_definition              |>> GroupingBodyStatement.FromDataDefinition)

        let parse_grouping_statement_implementation : Parser<GroupingStatement, 'a> =
            //[RFC 7950, p. 193]
            //grouping-stmt       = grouping-keyword sep identifier-arg-str optsep
            //                        (";" /
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            [status-stmt]
            //                            [description-stmt]
            //                            [reference-stmt]
            //                            *(typedef-stmt / grouping-stmt)
            //                            *data-def-stmt
            //                            *action-stmt
            //                            *notification-stmt
            //                        "}") stmtsep
            skipString "grouping" >>. spaces >>.
            Identifier.parse_identifier .>> spaces .>>.
            (
                    (skipChar ';'                               |>> (fun _ -> None))
                <|> (skipChar '{' >>. spaces >>.
                     (many parse_grouping_body_statement) .>>
                     spaces .>> skipChar '}'
                     |>> Some
                    ) .>> spaces
            )

        let parse_list_body_statement : Parser<ListBodyStatement, 'a> =
            // TODO: fill in missing parsing for ListBodyStatement
                (parse_key_statement            |>> ListBodyStatement.Key)
            <|> (parse_ordered_by_statement     |>> ListBodyStatement.OrderedBy)
            <|> (parse_status_statement         |>> ListBodyStatement.Status)
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
                (parse_status_statement         |>> UsesBodyStatement.Status)
            <|> (parse_description_statement    |>> UsesBodyStatement.Description)
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

        let parse_choice_body_statement : Parser<ChoiceBodyStatement, 'a> =
            // TODO: fill in missing parsing for ChoiceBodyStatement
                (parse_config_statement         |>> ChoiceBodyStatement.Config)
            <|> (parse_status_statement         |>> ChoiceBodyStatement.Status)
            <|> (parse_description_statement    |>> ChoiceBodyStatement.Description)
            <|> (parse_reference_statement      |>> ChoiceBodyStatement.Reference)
            <|> (parse_choice_statement         |>> ChoiceBodyStatement.Choice)
            <|> (parse_container_statement      |>> ChoiceBodyStatement.Container)
            <|> (parse_leaf_list                |>> ChoiceBodyStatement.LeafList)
            <|> (parse_leaf                     |>> ChoiceBodyStatement.Leaf)
            <|> (parse_list_statement           |>> ChoiceBodyStatement.List)
            <|> (parse_unknown_statement        |>> ChoiceBodyStatement.Unknown)

        let parse_choice_statement_implementation : Parser<ChoiceStatement, 'a> =
            // [RFC 7950, p. 196]
            //choice-stmt         = choice-keyword sep identifier-arg-str optsep
            //                        (";" /
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            [when-stmt]
            //                            *if-feature-stmt
            //                            [default-stmt]
            //                            [config-stmt]
            //                            [mandatory-stmt]
            //                            [status-stmt]
            //                            [description-stmt]
            //                            [reference-stmt]
            //                            *(short-case-stmt / case-stmt)
            //                        "}") stmtsep

            //short-case-stmt     = choice-stmt /
            //                        container-stmt /
            //                        leaf-stmt /
            //                        leaf-list-stmt /
            //                        list-stmt /
            //                        anydata-stmt /
            //                        anyxml-stmt
            skipString "choice" >>. spaces >>.
            Identifier.parse_identifier .>> spaces .>>.
            (
                    (skipChar ';' |>> (fun _ -> None))
                <|> (skipChar '{' >>. spaces >>.
                     (many parse_choice_body_statement) .>> spaces .>>
                     skipChar '}'
                     |>> Some
                    ) .>> spaces
            )

        let parse_data_definition_implementation : Parser<BodyStatement, 'a> =
            // TODO: fill in missing parsing for data-def-stmt
                (parse_container_statement  |>> BodyStatement.Container)
            <|> (parse_leaf_list            |>> BodyStatement.LeafList)
            <|> (parse_leaf                 |>> BodyStatement.Leaf)
            <|> (parse_list_statement       |>> BodyStatement.List)
            <|> (parse_choice_statement     |>> BodyStatement.Choice)
            <|> (parse_uses_statement       |>> BodyStatement.Uses)

        parse_data_definition_ref   := parse_data_definition_implementation
        parse_grouping_ref          := parse_grouping_statement_implementation
        parse_choice_statement_ref  := parse_choice_statement_implementation

        let parse_body : Parser<BodyStatement, 'a> =
            // TODO: fill in missing parsing for body-stmt
                (parse_typedef_statement                    |>> BodyStatement.TypeDef)
            <|> (parse_grouping_statement_implementation    |>> BodyStatement.Grouping)
            <|> parse_data_definition_implementation

        {
            Body            = parse_body
            DataDefinition  = parse_data_definition
            ContainerBody   = parse_container_body_statement
            Container       = parse_container_statement
        }

    let parse_body_statement<'a> : Parser<BodyStatement, 'a> = parsers.Body
    let parse_body_statements<'a> : Parser<BodyStatement list, 'a> = many parsers.Body

    let parse_container_body_statement<'a> : Parser<ContainerBodyStatement, 'a> = parsers.ContainerBody
    let parse_container_statement<'a> : Parser<ContainerStatement, 'a> = parsers.Container
