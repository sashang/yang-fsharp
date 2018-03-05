// Header.fs
// Definitions and parsing for YANG module header

namespace Yang.Parser

/// Type definitions and parsers for YANG module header
[<AutoOpen>]
module Header =
    open System
    open FParsec
    open Identifier
    open Yang.Model

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

    /// Parses all header statements
    let parse_header<'a> : Parser<ModuleHeaderStatements, 'a> =
        // TODO: check that there is exactly one definition of each header statement.
        // [RFC 7950, Sec. 7.1.1, p. 56] specifies that the cardinality of
        // yang-version, prefix, and namespace is 1.
        // This can be done by having mutable variables and checking at the end of the parser.

        /// Element parser
        let elementParser = spaces >>. parse_header_body_statement

        /// Create the state from the first element
        let stateFromFirstElement = function
        | YangVersion   e   -> Some e, None, None, None
        | Namespace     e   -> None, Some e, None, None
        | Prefix        e   -> None, None, Some e, None
        | Unknown       e   -> None, None, None, Some [e]

        /// Update state from element
        let foldState state element =
            match state, element with
            | (None, ns, ps, us), YangVersion e -> Some e, ns, ps, us
            | (Some _, _, _, _),  YangVersion e ->
                raise (YangParserException "Detected duplicate yang-version statement in module")

            | (vs, None, ps, us), Namespace e   -> vs, Some e, ps, us
            | (_, Some _, _, _),  Namespace e   ->
                raise (YangParserException "Detected duplicate namespace statement in module")

            | (vs, ns, None, us), Prefix e      -> vs, ns, Some e, us
            | (_, _, Some _, _),  Prefix e      ->
                raise (YangParserException "Detected duplicate prefix statement in module")

            | (vs, ns, ps, None), Unknown e     -> vs, ns, ps, Some [e]
            | (vs, ns, ps, Some us),  Unknown e -> vs, ns, ps, Some (us @ [e])

        let result state =
            match state with
            | Some vs, Some ns, Some ps, us -> vs, ns, ps, us
            | _                             -> raise (YangParserException "Failed to find all mandatory header statements for module")

        ParserHelper.ConsumeMany(
            elementParser           = elementParser,
            stateFromFirstElement   = stateFromFirstElement,
            foldState               = foldState,
            resultFromState         = result
        )
