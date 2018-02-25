// Header.fs
// Definitions and parsing for YANG module header

namespace Yang.Parser

/// Type definitions and parsers for YANG module header
[<AutoOpen>]
module Header =
    open System
    open FParsec
    open Identifier

    /// Module header information
    type Header = {
        /// Version of YANG used for the specification; should be 1.1
        YangVersion : YangVersionStatement

        /// Namespace of module
        Namespace : NamespaceStatement

        /// Prefix
        Prefix : PrefixStatement

        /// Extra unparsed statements that appear in the body.
        /// (those statements are kept in order, but caller should
        /// not make any assumption of position of description and
        /// reference statements)
        Options : ExtraStatements
    }
    with
        /// Get an empty module header
        static member Empty = {
            YangVersion = Version(1, 1), None
            // TODO: Is there a default URI for unspecified URI's?
            Namespace   = Uri("empty:"), None
            Prefix      = String.Empty, None
            Options     = None
        }

    type private HeaderStatement =
    | YangVersion   of YangVersionStatement
    | Namespace     of NamespaceStatement
    | Prefix        of PrefixStatement
    | Unknown       of UnknownStatement

    /// Parse a header statement
    let private parse_header_body_statement<'a> : Parser<HeaderStatement, 'a> =
            (parse_yang_version_statement   |>> YangVersion)
        <|> (parse_namespace_statement      |>> Namespace)
        <|> (parse_prefix_statement         |>> Prefix)
        <|> (parse_unknown_statement        |>> Unknown)

    /// Parser for end of header section
    let private end_of_header_parser<'a> : Parser<unit, 'a> =
        // Not currently used, but keeping around just in case.
        Identifier.RegularExpressions.notFollowedBy_keyword_or_prefix_identifier [ "yang-version"; "namespace"; "prefix" ]

    /// Parses all header statements
    let parse_header<'a> : Parser<Header, 'a> =
        // TODO: check that there is exactly one definition of each header statement.
        // [RFC 7950, Sec. 7.1.1, p. 56] specifies that the cardinality of
        // yang-version, prefix, and namespace is 1.
        // This can be done by having mutable variables and checking at the end of the parser.

        /// Element parser
        let elementParser = spaces >>. parse_header_body_statement

        /// Create the state from the first element
        let stateFromFirstElement = function
            | YangVersion   e   -> { Header.Empty with YangVersion  = e }
            | Namespace     e   -> { Header.Empty with Namespace    = e }
            | Prefix        e   -> { Header.Empty with Prefix       = e }
            | Unknown       e   -> { Header.Empty with Options      = Some [ Statements.Unknown e ] }

        /// Update state from element
        let foldState state element =
            match element with
            | YangVersion   e   -> { state with YangVersion   = e }
            | Namespace     e   -> { state with Namespace     = e }
            | Prefix        e   -> { state with Prefix        = e }
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
