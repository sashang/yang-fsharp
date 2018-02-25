// Meta.fs
// Definitions and parsing for YANG meta statements

namespace Yang.Parser

/// Type definitions and parsers for YANG meta statements
[<AutoOpen>]
module Meta =
    open System
    open FParsec
    open Identifier

    type Meta = {
        Organization    : OrganizationStatement option
        Contact         : ContactStatement option
        Description     : DescriptionStatement option
        Reference       : ReferenceStatement option

        /// Extra unparsed statements that appear in the body.
        /// (those statements are kept in order, but caller should
        /// not make any assumption of position of description and
        /// reference statements)
        Options : ExtraStatements
    }
    with
        static member Empty = {
            Organization    = None
            Contact         = None
            Description     = None
            Reference       = None
            Options         = None
        }

    type private MetaStatement =
    | Organization  of OrganizationStatement
    | Contact       of ContactStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement

    /// Parse a meta statement
    let private parse_meta_body_statement<'a> : Parser<MetaStatement, 'a> =
            (parse_organization_statement   |>> Organization)
        <|> (parse_contact_statement        |>> Contact)
        <|> (parse_description_statement    |>> Description)
        <|> (parse_reference_statement      |>> Reference)
        <|> (parse_unknown_statement        |>> Unknown)

    /// Parses all meta statements
    let parse_meta<'a> : Parser<Meta, 'a> =
        // TODO: check that there is exactly one definition of each header statement.
        // [RFC 7950, Sec. 7.1.1, p. 56] specifies that the cardinality of
        // organization, contact, description, and reference is either 0 or 1.
        // This can be done by having mutable variables and checking at the end of the parser.

        /// Element parser
        let elementParser = spaces >>. parse_meta_body_statement

        /// Create the state from the first element
        let stateFromFirstElement = function
            | Organization  e   -> { Meta.Empty with Organization  = Some e }
            | Contact       e   -> { Meta.Empty with Contact       = Some e }
            | Description   e   -> { Meta.Empty with Description   = Some e }
            | Reference     e   -> { Meta.Empty with Reference     = Some e }
            | Unknown       e   -> { Meta.Empty with Options       = Some [ Statements.Unknown e ] }

        /// Update state from element
        let foldState state element =
            match element with
            | Organization  e   -> { state with Organization  = Some e }
            | Contact       e   -> { state with Contact       = Some e }
            | Description   e   -> { state with Description   = Some e }
            | Reference     e   -> { state with Reference     = Some e }
            | Unknown       e   ->
                match state.Options with
                | None          -> { state with Options = Some [ Statements.Unknown e ] }
                | Some op       -> { state with Options = Some ( op @ [Statements.Unknown e] ) }

        ParserHelper.ConsumeMany(
            elementParser           = elementParser,
            stateFromFirstElement   = stateFromFirstElement,
            foldState               = foldState,
            resultFromState         = id
        )
