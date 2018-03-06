// Meta.fs
// Definitions and parsing for YANG meta statements

namespace Yang.Parser

/// Type definitions and parsers for YANG meta statements
[<AutoOpen>]
module Meta =
    open System
    open FParsec
    open Identifier
    open Yang.Model

    /// Parse a meta statement
    let private parse_meta_body_statement<'a> : Parser<MetaBodyStatement, 'a> =
            (parse_organization_statement   |>> MetaBodyStatement.Organization)
        <|> (parse_contact_statement        |>> MetaBodyStatement.Contact)
        <|> (parse_description_statement    |>> MetaBodyStatement.Description)
        <|> (parse_reference_statement      |>> MetaBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> MetaBodyStatement.Unknown)

    /// Parses all meta statements
    let parse_meta<'a> : Parser<MetaStatements, 'a> =
        /// Element parser
        let elementParser = spaces >>. parse_meta_body_statement

        /// Create the state from the first element
        let stateFromFirstElement = function
        | Organization              _ as e  -> [e], (true, false, false, false)
        | Contact                   _ as e  -> [e], (false, true, false, false)
        | Description               _ as e  -> [e], (false, false, true, false)
        | Reference                 _ as e  -> [e], (false, false, false, true)
        | MetaBodyStatement.Unknown _ as e  -> [e], (false, false, false, false)

        /// Update state from element
        let foldState state element =
            // The meta statements can appear at most once, so we keep track of which
            // we have seen, and fail if we see duplicates.

            match state, element with
            | (st, (false, co, de, re)), (Organization _)   -> st @ [element], (true, co, de, re)
            | (_,  (true, _, _, _)),     (Organization _)   ->
                raise (YangParserException "Detected duplicate organization statement in meta data")

            | (st, (og, false, de, re)), (Contact _)        -> st @ [element], (og, true, de, re)
            | (_,  (_, true, _, _)),     (Contact _)        ->
                raise (YangParserException "Detected duplicate contact statement in meta data")

            | (st, (og, co, false, de)), (Description _)    -> st @ [element], (og, co, true, de)
            | (_,  (_, _, true, _)),     (Description _)    ->
                raise (YangParserException "Detected duplicate description statement in meta data")

            | (st, (og, co, de, false)), (Reference _)      -> st @ [element], (og, co, de, true)
            | (_,  (_, _, _, true)),     (Reference _)      ->
                raise (YangParserException "Detected duplicate reference statement in meta data")

            | (st, flags), (MetaBodyStatement.Unknown _)    -> st @ [element], flags

        let result (state, _) = state

        ParserHelper.ConsumeMany(
            elementParser           = elementParser,
            stateFromFirstElement   = stateFromFirstElement,
            foldState               = foldState,
            resultFromState         = result,
            resultForEmptySequence  = fun _ -> []
        )
