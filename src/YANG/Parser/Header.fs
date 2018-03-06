// Header.fs
// Definitions and parsing for YANG module header

namespace Yang.Parser

/// Type definitions and parsers for YANG module header
[<AutoOpen>]
module Header =
    open System
    open System.Text
    open FParsec
    open NLog
    open Yang.Model

    /// Logger for this module
    let private _logger = LogManager.GetCurrentClassLogger()

#if INTERACTIVE
    // The following are used only in interactive (fsi) to help with enabling disabling
    // logging for particular modules.

    type internal Marker = interface end
    let _full_name = typeof<Marker>.DeclaringType.FullName
    let _name = typeof<Marker>.DeclaringType.Name
#endif

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
        /// Element parser
        let elementParser = spaces >>. parse_header_body_statement

        /// Create the state from the first element
        let stateFromFirstElement = function
        // We keep track of which elements we have seen, to make sure that they appear exactly once.
        | YangVersion   e   -> Some e, None, None, None
        | Namespace     e   -> None, Some e, None, None
        | Prefix        e   -> None, None, Some e, None
        | Unknown       e   -> None, None, None, Some [e]

        /// Update state from element
        let foldState state element =
            match state, element with
            | (None, ns, ps, us), YangVersion e -> Some e, ns, ps, us
            | (Some _, _, _, _),  YangVersion e ->
                _logger.Error("Detected duplicate yang-version statement in module")
                raise (YangParserException "Detected duplicate yang-version statement in module")

            | (vs, None, ps, us), Namespace e   -> vs, Some e, ps, us
            | (_, Some _, _, _),  Namespace e   ->
                _logger.Error("Detected duplicate namespace statement in module")
                raise (YangParserException "Detected duplicate namespace statement in module")

            | (vs, ns, None, us), Prefix e      -> vs, ns, Some e, us
            | (_, _, Some _, _),  Prefix e      ->
                _logger.Error("Detected duplicate prefix statement in module")
                raise (YangParserException "Detected duplicate prefix statement in module")

            | (vs, ns, ps, None), Unknown e     -> vs, ns, ps, Some [e]
            | (vs, ns, ps, Some us),  Unknown e -> vs, ns, ps, Some (us @ [e])

        let result state =
            match state with
            | Some vs, Some ns, Some ps, us -> vs, ns, ps, us
            | None,    Some ns, Some ps, us ->
                // Special case: the yang-version statement must appear, but there are cases where it is omitted.
                // So we will generate it.
                _logger.Warn("Module missing yang-version statement; will assume 1.1")
                let vs = Version(1, 1), None
                vs, ns, ps, us
            | _                             ->
                let sb = StringBuilder("Failed to find all mandatory header statements for module; missing:")
                let (vs, ns, ps, _) = state
                if vs.IsNone then Printf.bprintf sb " yang-version"
                if ns.IsNone then Printf.bprintf sb " namespace"
                if ps.IsNone then Printf.bprintf sb " prefix"

                _logger.Error("Error parsing header statements: Missing header statements; got state:\n{0}", state)

                raise (YangParserException (sb.ToString()))

        ParserHelper.ConsumeMany(
            elementParser           = elementParser,
            stateFromFirstElement   = stateFromFirstElement,
            foldState               = foldState,
            resultFromState         = result
        )
