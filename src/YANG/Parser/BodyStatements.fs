// DataDefinitions.fs
// Definitions and parsing for YANG defined types

namespace Yang.Parser

module BodyStatements =
    open FParsec
    open Yang.Model
    open Leaf
    open LeafList
    open Yang.Model.Arguments
    open Yang.Parser.Expressions

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
        Action          : Parser<ActionStatement, 'a>
        Augment         : Parser<AugmentStatement, 'a>
        Body            : Parser<BodyStatement, 'a>
        Case            : Parser<CaseStatement, 'a>
        Choice          : Parser<ChoiceStatement, 'a>
        Container       : Parser<ContainerStatement, 'a>
        ContainerBody   : Parser<ContainerBodyStatement, 'a>
        DataDefinition  : Parser<BodyStatement, 'a>
        Grouping        : Parser<GroupingStatement, 'a>
        Input           : Parser<InputStatement, 'a>
        List            : Parser<ListStatement, 'a>
        Notification    : Parser<NotificationStatement, 'a>
        Output          : Parser<OutputStatement, 'a>
        Rpc             : Parser<RpcStatement, 'a>
        TypeDef         : Parser<TypeDefStatement, 'a>
        Uses            : Parser<UsesStatement, 'a>
        UsesAugment     : Parser<UsesAugmentStatement, 'a>
    }

    /// Create parsers for the high-level constructs of the model (data  types, etc).
    /// All parsers created below depend on each other.
    let private parsers<'a> =
        //
        // First, create some placeholders for parsers in order to break the dependencies.
        //

        let (parse_action_statement : Parser<ActionStatement, 'a>), (parse_action_statement_ref : Parser<ActionStatement, 'a> ref) =
            createParserForwardedToRef<ActionStatement, 'a>()

        let (parse_data_definition : Parser<BodyStatement, 'a>), (parse_data_definition_ref : Parser<BodyStatement, 'a> ref) =
            createParserForwardedToRef<BodyStatement, 'a>()

        let (parse_grouping_statement : Parser<GroupingStatement, 'a>), (parse_grouping_statement_ref : Parser<GroupingStatement, 'a> ref) =
            createParserForwardedToRef<GroupingStatement, 'a>()

        let (parse_choice_statement : Parser<ChoiceStatement, 'a>), (parse_choice_statement_ref : Parser<ChoiceStatement, 'a> ref) =
            createParserForwardedToRef<ChoiceStatement, 'a>()

        //
        // Next, create the parsers for the various statements
        //
        // Make sure that when parse_uses_statement appears, it is in the end,
        // and that leaf-list is parsed before leaf

        let parse_typedef_body_statement : Parser<TypeDefBodyStatement, 'a> =
                (Types.parse_type_statement     |>> TypeDefBodyStatement.Type)
            <|> (parse_units_statement          |>> TypeDefBodyStatement.Units)
            <|> (parse_default_statement        |>> TypeDefBodyStatement.Default)
            <|> (parse_status_statement         |>> TypeDefBodyStatement.Status)
            <|> (parse_description_statement    |>> TypeDefBodyStatement.Description)
            <|> (parse_reference_statement      |>> TypeDefBodyStatement.Reference)
            <|> (parse_unknown_statement        |>> TypeDefBodyStatement.Unknown)

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
            make_statement_parser_generic "typedef" (pip Strings.parse_string Identifier.parse_identifier) parse_typedef_body_statement
            |>> TypeDefStatement

        let parse_notification_body_statement : Parser<NotificationBodyStatement, 'a> =
                (parse_if_feature_statement     |>> NotificationBodyStatement.IfFeature)
            <|> (parse_must_statement           |>> NotificationBodyStatement.Must)
            <|> (parse_status_statement         |>> NotificationBodyStatement.Status)
            <|> (parse_description_statement    |>> NotificationBodyStatement.Description)
            <|> (parse_reference_statement      |>> NotificationBodyStatement.Reference)
            <|> (parse_typedef_statement        |>> NotificationBodyStatement.TypeDef)
            <|> (parse_grouping_statement       |>> NotificationBodyStatement.Grouping)
            <|> (parse_data_definition          |>> NotificationBodyStatement.FromDataDefinition)
            <|> (parse_unknown_statement        |>> NotificationBodyStatement.Unknown)

        let parse_notification_statement : Parser<NotificationStatement, 'a> =
            // [RFC 7950, p. 200]
            //notification-stmt   = notification-keyword sep
            //                        identifier-arg-str optsep
            //                        (";" /
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            *if-feature-stmt
            //                            *must-stmt
            //                            [status-stmt]
            //                            [description-stmt]
            //                            [reference-stmt]
            //                            *(typedef-stmt / grouping-stmt)
            //                            *data-def-stmt
            //                        "}") stmtsep
            // TODO: Check and enforce cardinality constraints for notification-stmt
            make_statement_parser_optional_generic "notification" (pip Strings.parse_string Identifier.parse_identifier) parse_notification_body_statement
            |>> NotificationStatement

        let parse_grouping_body_statement : Parser<GroupingBodyStatement, 'a> =
                (parse_status_statement             |>> GroupingBodyStatement.Status)
            <|> (parse_description_statement        |>> GroupingBodyStatement.Description)
            <|> (parse_reference_statement          |>> GroupingBodyStatement.Reference)
            <|> (parse_typedef_statement            |>> GroupingBodyStatement.TypeDef)
            <|> (parse_grouping_statement           |>> GroupingBodyStatement.Grouping)
            <|> (parse_action_statement             |>> GroupingBodyStatement.Action)
            <|> (parse_notification_statement       |>> GroupingBodyStatement.Notification)
            <|> (parse_data_definition              |>> GroupingBodyStatement.FromDataDefinition)
            <|> (parse_unknown_statement            |>> GroupingBodyStatement.Unknown)

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
            // TODO: Check and enforce cardinality constraints for grouping-stmt
            make_statement_parser_optional_generic "grouping" (pip Strings.parse_string Identifier.parse_identifier) parse_grouping_body_statement
            |>> GroupingStatement

        let parse_input_body_statement : Parser<InputBodyStatement, 'a> =
                (parse_must_statement           |>> InputBodyStatement.Must)
            <|> (parse_typedef_statement        |>> InputBodyStatement.TypeDef)
            <|> (parse_grouping_statement       |>> InputBodyStatement.Grouping)
            <|> (parse_data_definition          |>> InputBodyStatement.FromDataDefinition)
            <|> (parse_unknown_statement        |>> InputBodyStatement.Unknown)

        let parse_input_statement : Parser<InputStatement, 'a> =
            //[RFC 7950, p. 200]
            //input-stmt          = input-keyword optsep
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            *must-stmt
            //                            *(typedef-stmt / grouping-stmt)
            //                            1*data-def-stmt
            //                        "}" stmtsep
            make_statement_parser_no_argument_generic "input" parse_input_body_statement
            |>> InputStatement

        let parse_list_body_statement : Parser<ListBodyStatement, 'a> =
                (parse_when_statement           |>> ListBodyStatement.When)
            <|> (parse_if_feature_statement     |>> ListBodyStatement.IfFeature)
            <|> (parse_must_statement           |>> ListBodyStatement.Must)
            <|> (parse_key_statement            |>> ListBodyStatement.Key)
            <|> (parse_unique_statement         |>> ListBodyStatement.Unique)
            <|> (parse_config_statement         |>> ListBodyStatement.Config)
            <|> (parse_min_elements_statement   |>> ListBodyStatement.MinElements)
            <|> (parse_max_elements_statement   |>> ListBodyStatement.MaxElements)
            <|> (parse_ordered_by_statement     |>> ListBodyStatement.OrderedBy)
            <|> (parse_status_statement         |>> ListBodyStatement.Status)
            <|> (parse_description_statement    |>> ListBodyStatement.Description)
            <|> (parse_reference_statement      |>> ListBodyStatement.Reference)
            <|> (parse_typedef_statement        |>> ListBodyStatement.TypeDef)
            <|> (parse_grouping_statement       |>> ListBodyStatement.Grouping)
            <|> (parse_action_statement         |>> ListBodyStatement.Action)
            <|> (parse_notification_statement   |>> ListBodyStatement.Notification)
            <|> (parse_data_definition          |>> ListBodyStatement.FromDataDefinition)
            <|> (parse_unknown_statement        |>> ListBodyStatement.Unknown)

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
            // TODO: Check and enforce cardinality constraints for list-stmt
            make_statement_parser_generic "list" (pip Strings.parse_string Identifier.parse_identifier) parse_list_body_statement
            |>> ListStatement

        let parse_output_body_statement : Parser<OutputBodyStatement, 'a> =
                (parse_must_statement           |>> OutputBodyStatement.Must)
            <|> (parse_typedef_statement        |>> OutputBodyStatement.TypeDef)
            <|> (parse_grouping_statement       |>> OutputBodyStatement.Grouping)
            <|> (parse_data_definition          |>> OutputBodyStatement.FromDataDefinition)
            <|> (parse_unknown_statement        |>> OutputBodyStatement.Unknown)

        let parse_output_statement : Parser<OutputStatement, 'a> =
            //[RFC 7950, p. 200]
            //input-stmt          = output-keyword optsep
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            *must-stmt
            //                            *(typedef-stmt / grouping-stmt)
            //                            1*data-def-stmt
            //                        "}" stmtsep
            make_statement_parser_no_argument_generic "output" parse_output_body_statement
            |>> OutputStatement

        let parse_rpc_body_statement : Parser<RpcBodyStatement, 'a> =
                (parse_if_feature_statement         |>> RpcBodyStatement.IfFeature)
            <|> (parse_status_statement             |>> RpcBodyStatement.Status)
            <|> (parse_description_statement        |>> RpcBodyStatement.Description)
            <|> (parse_reference_statement          |>> RpcBodyStatement.Reference)
            <|> (parse_typedef_statement            |>> RpcBodyStatement.TypeDef)
            <|> (parse_grouping_statement           |>> RpcBodyStatement.Grouping)
            <|> (parse_input_statement              |>> RpcBodyStatement.Input)
            <|> (parse_output_statement             |>> RpcBodyStatement.Output)
            <|> (parse_unknown_statement            |>> RpcBodyStatement.Unknown)

        let parse_rpc_statement : Parser<RpcStatement, 'a> =
            //[RFC 7950, p. 199]
            //rpc-stmt            = rpc-keyword sep identifier-arg-str optsep
            //                        (";" /
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            *if-feature-stmt
            //                            [status-stmt]
            //                            [description-stmt]
            //                            [reference-stmt]
            //                            *(typedef-stmt / grouping-stmt)
            //                            [input-stmt]
            //                            [output-stmt]
            //                        "}") stmtsep
            make_statement_parser_optional_generic "rpc" (pip Strings.parse_string Identifier.parse_identifier) parse_rpc_body_statement
            |>> RpcStatement

        let parse_action_body_statement : Parser<ActionBodyStatement, 'a> =
                (parse_if_feature_statement     |>> ActionBodyStatement.IfFeature)
            <|> (parse_status_statement         |>> ActionBodyStatement.Status)
            <|> (parse_description_statement    |>> ActionBodyStatement.Description)
            <|> (parse_reference_statement      |>> ActionBodyStatement.Reference)
            <|> (parse_typedef_statement        |>> ActionBodyStatement.TypeDef)
            <|> (parse_grouping_statement       |>> ActionBodyStatement.Grouping)
            <|> (parse_input_statement          |>> ActionBodyStatement.Input)
            <|> (parse_output_statement         |>> ActionBodyStatement.Output)
            <|> (parse_unknown_statement        |>> ActionBodyStatement.Unknown)

        let parse_action_statement_implementation : Parser<ActionStatement, 'a> =
            // [RFC 7950, p. 200]
            //action-stmt         = action-keyword sep identifier-arg-str optsep
            //                        (";" /
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            *if-feature-stmt
            //                            [status-stmt]
            //                            [description-stmt]
            //                            [reference-stmt]
            //                            *(typedef-stmt / grouping-stmt)
            //                            [input-stmt]
            //                            [output-stmt]
            //                        "}") stmtsep
            // TODO: Check and enforce cardinality constraints for action-stmt
            make_statement_parser_optional_generic "action" (pip Strings.parse_string Identifier.parse_identifier) parse_action_body_statement
            |>> ActionStatement

        let parse_case_body_statement : Parser<CaseBodyStatement, 'a> =
                (parse_when_statement           |>> CaseBodyStatement.When)
            <|> (parse_if_feature_statement     |>> CaseBodyStatement.IfFeature)
            <|> (parse_status_statement         |>> CaseBodyStatement.Status)
            <|> (parse_description_statement    |>> CaseBodyStatement.Description)
            <|> (parse_reference_statement      |>> CaseBodyStatement.Reference)
            <|> (parse_data_definition          |>> CaseBodyStatement.FromDataDefinition)
            <|> (parse_unknown_statement        |>> CaseBodyStatement.Unknown)

        let parse_case_statement : Parser<CaseStatement, 'a> =
            // [RFC 7950, p.196]
            //case-stmt           = case-keyword sep identifier-arg-str optsep
            //                        (";" /
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            [when-stmt]
            //                            *if-feature-stmt
            //                            [status-stmt]
            //                            [description-stmt]
            //                            [reference-stmt]
            //                            *data-def-stmt
            //                        "}") stmtsep
            // TODO: Check and enforce cardinality for case-stmt
            make_statement_parser_optional_generic "case" (pip Strings.parse_string Identifier.parse_identifier) parse_case_body_statement
            |>> CaseStatement

        let parse_augment_body_statement : Parser<AugmentBodyStatement, 'a> =
                (parse_when_statement           |>> AugmentBodyStatement.When)
            <|> (parse_if_feature_statement     |>> AugmentBodyStatement.IfFeature)
            <|> (parse_status_statement         |>> AugmentBodyStatement.Status)
            <|> (parse_description_statement    |>> AugmentBodyStatement.Description)
            <|> (parse_reference_statement      |>> AugmentBodyStatement.Reference)
            <|> (parse_case_statement           |>> AugmentBodyStatement.Case)
            <|> (parse_action_statement         |>> AugmentBodyStatement.Action)
            <|> (parse_notification_statement   |>> AugmentBodyStatement.Notification)
            <|> (parse_data_definition          |>> AugmentBodyStatement.FromDataDefinition)
            <|> (parse_unknown_statement        |>> AugmentBodyStatement.Unknown)

        let parse_augment_statement : Parser<AugmentStatement, 'a> =
            // [RFC 7950, p.199]
            //augment-stmt        = augment-keyword sep augment-arg-str optsep
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            [when-stmt]
            //                            *if-feature-stmt
            //                            [status-stmt]
            //                            [description-stmt]
            //                            [reference-stmt]
            //                            1*(data-def-stmt / case-stmt /
            //                            action-stmt / notification-stmt)
            //                        "}" stmtsep
            make_statement_parser_generic "augment" (pip Strings.parse_string Identifier.parse_schema_node_identifier_absolute) parse_augment_body_statement
            |>> fun (augment, body) -> AugmentStatement (Augment augment, body)

        let parse_container_body_statement : Parser<ContainerBodyStatement, 'a> =
                (parse_when_statement           |>> ContainerBodyStatement.When)
            <|> (parse_if_feature_statement     |>> ContainerBodyStatement.IfFeature)
            <|> (parse_must_statement           |>> ContainerBodyStatement.Must)
            <|> (parse_presence_statement       |>> ContainerBodyStatement.Presence)
            <|> (parse_config_statement         |>> ContainerBodyStatement.Config)
            <|> (parse_status_statement         |>> ContainerBodyStatement.Status)
            <|> (parse_description_statement    |>> ContainerBodyStatement.Description)
            <|> (parse_reference_statement      |>> ContainerBodyStatement.Reference)
            <|> (parse_typedef_statement        |>> ContainerBodyStatement.TypeDef)
            <|> (parse_grouping_statement       |>> ContainerBodyStatement.Grouping)
            <|> (parse_action_statement         |>> ContainerBodyStatement.Action)
            <|> (parse_notification_statement   |>> ContainerBodyStatement.Notification)
            <|> (parse_data_definition          |>> ContainerBodyStatement.FromDataDefinition)
            <|> (parse_unknown_statement        |>> ContainerBodyStatement.Unknown)

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
            // TODO: Check and enforce cardinality for container-stmt.
            make_statement_parser_optional_generic "container" (pip Strings.parse_string Identifier.parse_identifier) parse_container_body_statement
            |>> ContainerStatement

        let parse_choice_body_statement : Parser<ChoiceBodyStatement, 'a> =
                (parse_when_statement           |>> ChoiceBodyStatement.When)
            <|> (parse_if_feature_statement     |>> ChoiceBodyStatement.IfFeature)
            <|> (parse_default_statement        |>> ChoiceBodyStatement.Default)
            <|> (parse_config_statement         |>> ChoiceBodyStatement.Config)
            <|> (parse_mandatory_statement      |>> ChoiceBodyStatement.Mandatory)
            <|> (parse_status_statement         |>> ChoiceBodyStatement.Status)
            <|> (parse_description_statement    |>> ChoiceBodyStatement.Description)
            <|> (parse_reference_statement      |>> ChoiceBodyStatement.Reference)
            <|> (parse_choice_statement         |>> ChoiceBodyStatement.Choice)
            <|> (parse_container_statement      |>> ChoiceBodyStatement.Container)
            <|> (parse_leaf_list_statement      |>> ChoiceBodyStatement.LeafList)
            <|> (parse_leaf_statement           |>> ChoiceBodyStatement.Leaf)
            <|> (parse_list_statement           |>> ChoiceBodyStatement.List)
            <|> (parse_any_data_statement       |>> ChoiceBodyStatement.AnyData)
            <|> (parse_any_xml_statement        |>> ChoiceBodyStatement.AnyXml)
            <|> (parse_case_statement           |>> ChoiceBodyStatement.Case)
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
            // TODO: Check and enforce cardinality and other constraints for choice-stmt
            make_statement_parser_optional_generic "choice" (pip Strings.parse_string Identifier.parse_identifier) parse_choice_body_statement
            |>> ChoiceStatement

        let parse_uses_augment_body_statement : Parser<UsesAugmentBodyStatement, 'a> =
                (parse_when_statement           |>> UsesAugmentBodyStatement.When)
            <|> (parse_if_feature_statement     |>> UsesAugmentBodyStatement.IfFeature)
            <|> (parse_status_statement         |>> UsesAugmentBodyStatement.Status)
            <|> (parse_description_statement    |>> UsesAugmentBodyStatement.Description)
            <|> (parse_reference_statement      |>> UsesAugmentBodyStatement.Reference)
            <|> (parse_case_statement           |>> UsesAugmentBodyStatement.Case)
            <|> (parse_action_statement         |>> UsesAugmentBodyStatement.Action)
            <|> (parse_notification_statement   |>> UsesAugmentBodyStatement.Notification)
            <|> (parse_data_definition          |>> UsesAugmentBodyStatement.FromDataDefinition)
            <|> (parse_unknown_statement        |>> UsesAugmentBodyStatement.Unknown)

        let parse_uses_augment_statement : Parser<UsesAugmentStatement, 'a> =
            // [RFC 7950, p. 198]
            //uses-augment-stmt   = augment-keyword sep uses-augment-arg-str optsep
            //                        "{" stmtsep
            //                            ;; these stmts can appear in any order
            //                            [when-stmt]
            //                            *if-feature-stmt
            //                            [status-stmt]
            //                            [description-stmt]
            //                            [reference-stmt]
            //                            1*(data-def-stmt / case-stmt /
            //                            action-stmt / notification-stmt)
            //                        "}" stmtsep
            make_statement_parser_generic "augment" (pip Strings.parse_string Identifier.parse_schema_node_identifier_descendant) parse_uses_augment_body_statement
            |>> fun (augment, body) -> UsesAugmentStatement (UsesAugment augment, body)

        let parse_uses_body_statement : Parser<UsesBodyStatement, 'a> =
            // TODO: fill in missing parsing for UsesBodyStatement
                (parse_when_statement           |>> UsesBodyStatement.When)
            <|> (parse_if_feature_statement     |>> UsesBodyStatement.IfFeature)
            <|> (parse_status_statement         |>> UsesBodyStatement.Status)
            <|> (parse_description_statement    |>> UsesBodyStatement.Description)
            <|> (parse_reference_statement      |>> UsesBodyStatement.Reference)
            <|> (parse_refine_statement         |>> UsesBodyStatement.Refine)
            <|> (parse_uses_augment_statement   |>> UsesBodyStatement.UsesAugment)
            <|> (parse_unknown_statement        |>> UsesBodyStatement.Unknown)

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
            make_statement_parser_optional_generic "uses" (pip Strings.parse_string Identifier.parse_identifier_reference) parse_uses_body_statement
            |>> UsesStatement

        let parse_data_definition_implementation : Parser<BodyStatement, 'a> =
            // TODO: fill in missing parsing for data-def-stmt
                (parse_container_statement  |>> BodyStatement.Container)
            <|> (parse_leaf_list_statement  |>> BodyStatement.LeafList)
            <|> (parse_leaf_statement       |>> BodyStatement.Leaf)
            <|> (parse_list_statement       |>> BodyStatement.List)
            <|> (parse_choice_statement     |>> BodyStatement.Choice)
            <|> (parse_any_data_statement   |>> BodyStatement.AnyData)
            <|> (parse_any_xml_statement    |>> BodyStatement.AnyXml)
            <|> (parse_uses_statement       |>> BodyStatement.Uses)

        parse_action_statement_ref      := parse_action_statement_implementation
        parse_data_definition_ref       := parse_data_definition_implementation
        parse_grouping_statement_ref    := parse_grouping_statement_implementation
        parse_choice_statement_ref      := parse_choice_statement_implementation

        let parse_body : Parser<BodyStatement, 'a> =
                (parse_extension_statement                  |>> BodyStatement.Extension)
            <|> (parse_feature_statement                    |>> BodyStatement.Feature)
            <|> (parse_identity_statement                   |>> BodyStatement.Identity)
            <|> (parse_typedef_statement                    |>> BodyStatement.TypeDef)
            <|> (parse_grouping_statement_implementation    |>> BodyStatement.Grouping)
            <|> parse_data_definition_implementation
            <|> (parse_augment_statement                    |>> BodyStatement.Augment)
            <|> (parse_rpc_statement                        |>> BodyStatement.Rpc)
            <|> (parse_notification_statement               |>> BodyStatement.Notification)
            <|> (parse_deviation_statement                  |>> BodyStatement.Deviation)
            <|> (parse_unknown_statement                    |>> BodyStatement.Unknown)

        {
            Action          = parse_action_statement
            Augment         = parse_augment_statement
            Body            = parse_body
            Case            = parse_case_statement
            Choice          = parse_choice_statement
            Container       = parse_container_statement
            ContainerBody   = parse_container_body_statement
            DataDefinition  = parse_data_definition
            Grouping        = parse_grouping_statement
            Input           = parse_input_statement
            List            = parse_list_statement
            Notification    = parse_notification_statement
            Output          = parse_output_statement
            Rpc             = parse_rpc_statement
            TypeDef         = parse_typedef_statement
            Uses            = parse_uses_statement
            UsesAugment     = parse_uses_augment_statement
        }

    let parse_action_statement<'a>          : Parser<ActionStatement, 'a>           = parsers.Action
    let parse_augment_statement<'a>         : Parser<AugmentStatement, 'a>          = parsers.Augment
    let parse_body_statement<'a>            : Parser<BodyStatement, 'a>             = parsers.Body
    let parse_body_statements<'a>           : Parser<BodyStatement list, 'a>        = many parsers.Body
    let parse_case_statement<'a>            : Parser<CaseStatement, 'a>             = parsers.Case
    let parse_choice_statement<'a>          : Parser<ChoiceStatement, 'a>           = parsers.Choice
    let parse_container_body_statement<'a>  : Parser<ContainerBodyStatement, 'a>    = parsers.ContainerBody
    let parse_container_statement<'a>       : Parser<ContainerStatement, 'a>        = parsers.Container
    let parse_grouping_statement<'a>        : Parser<GroupingStatement, 'a>         = parsers.Grouping
    let parse_input_statement<'a>           : Parser<InputStatement, 'a>            = parsers.Input
    let parse_list_statement<'a>            : Parser<ListStatement, 'a>             = parsers.List
    let parse_notification_statement<'a>    : Parser<NotificationStatement, 'a>     = parsers.Notification
    let parse_output_statement<'a>          : Parser<OutputStatement, 'a>           = parsers.Output
    let parse_rpc_statement<'a>             : Parser<RpcStatement, 'a>              = parsers.Rpc
    let parse_typedef_statement<'a>         : Parser<TypeDefStatement, 'a>          = parsers.TypeDef
    let parse_uses_statement<'a>            : Parser<UsesStatement, 'a>             = parsers.Uses
    let parse_uses_augment_statement<'a>    : Parser<UsesAugmentStatement, 'a>      = parsers.UsesAugment
