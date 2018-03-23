// GenericParser.fs
// Parser for generic blocks, ie. blocks that can contain any statement

namespace Yang.Parser

[<AutoOpen>]
module GenericParser =
    open FParsec
    open Yang.Model
    open Yang.Parser.BodyStatements

    let generic_yang_statement_implementation<'a> : Parser<Statement, 'a> =
            (parse_action_statement             |>> Statement.Action)
        <|> (parse_config_statement             |>> Statement.Config)
        <|> (parse_contact_statement            |>> Statement.Contact)

        <|> (parse_description_statement        |>> Statement.Description)

        <|> (parse_error_app_tag_statement      |>> Statement.ErrorAppTag)
        <|> (parse_error_message_statement      |>> Statement.ErrorMessage)

        <|> (parse_namespace_statement          |>> Statement.Namespace)

        <|> (parse_organization_statement       |>> Statement.Organization)

        <|> (parse_prefix_statement             |>> Statement.Prefix)
        <|> (parse_presence_statement           |>> Statement.Presence)

        <|> (parse_reference_statement          |>> Statement.Reference)

        <|> (parse_unknown_statement            |>> Statement.Unknown)

    do
        generic_parser<unit>.Implementation := generic_yang_statement_implementation<unit>
