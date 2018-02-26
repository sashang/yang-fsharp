// Body.fs
// Definitions and parsing for the body-stmts

namespace Yang.Parser

/// Parsers and structures for body statements
[<AutoOpen>]
module Body =
    open FParsec

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

    type BodyItem =
    | DataDefinition of DataDefinitions.DataDefinitionStatement

    type Body = BodyItem list

    let private parse_body_item<'a> : Parser<BodyItem, 'a> =
        DataDefinitions.parse_data_definition |>> DataDefinition

    let parse_body<'a> : Parser<Body, 'a> = many parse_body_item
