// Statements.fs
namespace Yang.Parser

/// Parsers for common statements, that appear in the yang-stmt rule
[<AutoOpen>]
module Statements =
    open System
    open FParsec
    open Identifier
    open Identifier

    // [RFC 7950, p. 202-204]
    //
    // Many definitions end with the rule stmtend or stmtsep:
    //  stmtend             = optsep (";" / "{" stmtsep "}") stmtsep
    //  optsep              = *(WSP / line-break)
    //  stmtsep             = *(WSP / line-break / unknown-statement)
    //  unknown-statement   = prefix ":" identifier [sep string] optsep
    //                        (";" /
    //                         "{" optsep
    //                             *((yang-stmt / unknown-statement) optsep)
    //                         "}") stmtsep
    //
    // Observe that the rules above encourage to consume without processing all unknown-statement
    // definitions. However, we want to at least keep track of them, and give the user the option to
    // act upon them. Hence, we will use the following modified rules:
    //  stmtend             = optsep (";" / "{" stmtsep "}") optsep
    //  unknown-statement   = prefix ":" identifier [sep string] optsep
    //                        (";" /
    //                         "{" optsep
    //                             *((yang-stmt / unknown-statement) optsep)
    //                         "}") optsep
    // and also process unknown-statement rules.

    // TODO: Namespaces are hierarchical with colon as separator, e.g. urn:example:system

    /// Available Yang statement definitions
    [<StructuredFormatDisplay("{PrettyPrint}")>]
    type Statement =
    // Observe that many rules can be followed by blocks of unknown statements.
    // We store the statements in those blocks in a special field called Options.
    // Typically this should be empty.

    /// Contact statement
    | Contact       of Contact:string * Options:ExtraStatements
    | Description   of DescriptionStatement
    | Namespace     of NamespaceStatement
    | Organization  of Organization:string * Options:ExtraStatements
    | Prefix        of PrefixStatement
    | Reference     of ReferenceStatement
    | YangVersion   of YangVersionStatement
    | Unknown       of UnknownStatement
    | Unparsed      of Identifier:Identifier * Argument:(string option) * Body:(Statement list option)
    with
        member this.Options =
            match this with
            | Contact       (_, options)
            | Description   (_, options)
            | Namespace     (_, options)
            | Organization  (_, options)
            | Prefix        (_, options)
            | Reference     (_, options)
            | YangVersion   (_, options)
                -> options
            | Unknown _
            | Unparsed _
                -> None

        member this.Identifier =
            match this with
            | Contact _         -> "contact"
            | Description _     -> "description"
            | Namespace _       -> "namespace"
            | Organization _    -> "organization"
            | Prefix _          -> "prefix"
            | Reference _       -> "reference"
            | YangVersion _     -> "yang-version"
            | Unknown (id, _, _)    -> id.ToString()
            | Unparsed (id, _, _)   -> id.ToString()

        member this.PrettyPrint =
            let escape = [| ' '; '\t'; '\r'; '\n'; ';'; '{'; '}'; '@'; ':' |]

            /// Pretty print string
            let ps (input : string) =
                if input.IndexOfAny(escape) > 0 then sprintf "\"%s\"" input
                else input

            /// Pretty print optional string
            let pso (input : string option) =
                match input with
                | Some str ->
                    if str.IndexOfAny(escape) > 0 then sprintf "\"%s\"" str
                    else str
                | None -> ""

            /// Pretty print block
            let pb (options : Statement list option) =
                match options with
                | None -> ""
                | Some block -> sprintf "{ %A }" block

            let id = this.Identifier

            match this with
            | Contact (s, b)
            | Description (s, b)
            | Organization (s, b)
            | Prefix (s, b)
            | Reference (s, b)
                -> sprintf "%s %s %s" id (ps s) (pb b)

            | Namespace (uri, block)        -> sprintf "%s %s %s" id (uri.ToString()) (pb block)
            | YangVersion (version, block)  -> sprintf "%s %s %s" id (version.ToString()) (pb block)

            | Unknown (id, arg, body)       -> sprintf "Unknown: %A %s %s"  id (pso arg) (pb body)
            | Unparsed (id, arg, body)      -> sprintf "Unparsed: %A %s %s" id (pso arg) (pb body)
    /// Short name for the extra statements that may appear
    and ExtraStatements         = Statement list option
    and DescriptionStatement    = string * ExtraStatements
    and NamespaceStatement      = Uri * ExtraStatements
    and PrefixStatement         = string * ExtraStatements
    and ReferenceStatement      = string * ExtraStatements
    and YangVersionStatement    = Version * ExtraStatements
    and UnknownStatement        = IdentifierWithPrefix * (string option) * ExtraStatements

    // Helper definitions that consume trailing whitespace
    let inline private read_keyword<'a> : Parser<string, 'a> = Strings.parse_string .>> spaces
    let inline private end_of_statement<'a> : Parser<unit, 'a> = skipChar ';' >>. spaces
    let inline private begin_block<'a> : Parser<unit, 'a> = skipChar '{' .>> spaces
    let inline private end_block<'a> : Parser<unit, 'a> = spaces .>> skipChar '}' .>> spaces

    let inline block<'a> (parser : Parser<Statement, 'a>) : Parser<Statement list, 'a> =
        manyTill parser end_block

    let inline end_of_statement_or_block<'a> (parser : Parser<Statement, 'a>) : Parser<ExtraStatements, 'a> =
        (end_of_statement |>> (fun _ -> None))
        <|>
        (begin_block >>. (block parser) |>> (fun statements -> Some statements))

    let inline private unknown_statement<'a> (parser : Parser<Statement, 'a>) : Parser<Statement, 'a> =
            Identifier.parse_identifier_with_prefix
            .>> spaces
            .>>. ((end_of_statement_or_block parser
                   |>> (fun body            -> None, body))
                  <|>
                  (read_keyword .>>. end_of_statement_or_block parser
                   |>> (fun (argument, body)    -> Some argument, body))
                 )
            |>> (fun (identifier, (argument, body)) -> Unknown (identifier, argument, body))

    let inline private unparsed_statement<'a> (parser : Parser<Statement, 'a>) : Parser<Statement, 'a> =
            Identifier.parse_identifier
            .>> spaces
            .>>. ((end_of_statement_or_block parser
                   |>> (fun body            -> None, body))
                  <|>
                  (read_keyword .>>. end_of_statement_or_block parser
                   |>> (fun (argument, body)    -> Some argument, body))
                 )
            |>> (fun (identifier, (argument, body)) -> Unparsed (identifier, argument, body))

    let inline private yang_keyword_string_statement<'a> (keyword : string, maker) (parser : Parser<Statement, 'a>) : Parser<Statement, 'a> =
        skipStringCI keyword >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parser
        |>> maker

    let inline private yang_keyword_uri_statement<'a> (keyword : string, maker) (parser : Parser<Statement, 'a>) : Parser<Statement, 'a> =
        skipStringCI keyword >>. spaces >>.
        (Strings.parse_string |>> Uri) .>> spaces .>>.
        end_of_statement_or_block parser
        |>> maker

    /// Parses a yang-stmt [RFC 7950, p. 202].
    /// It should be used for parsing rules with no constraints, e.g.
    // inside unknown-statement rules.
    let inline parse_statement<'a> : Parser<Statement, 'a> =
        // This parser accepts any type of statement. Typically, it should be used for statements
        // in unknown statements that have no constaints. Because, of their generality they can
        // be applied recursively.

        let (parse_statement : Parser<Statement, 'a>), (parse_statement_ref : Parser<Statement, 'a> ref) =
            createParserForwardedToRef<Statement, 'a>()

        let inline parse_statement_implementation (input : CharStream<'a>) : Reply<Statement> =
            let parser =
                    yang_keyword_string_statement (
                        "yang-version",
                        (fun (version, options) ->
                            let version' = Version.Parse version
                            YangVersion (version', options)
                        )) parse_statement
                <|> yang_keyword_string_statement ("contact", Contact)              parse_statement
                <|> yang_keyword_string_statement ("description", Description)      parse_statement
                <|> yang_keyword_uri_statement    ("namespace", Namespace)          parse_statement
                <|> yang_keyword_string_statement ("organization", Organization)    parse_statement
                <|> yang_keyword_string_statement ("prefix", Prefix)                parse_statement
                <|> yang_keyword_string_statement ("reference", Reference)          parse_statement
                <|> unknown_statement parse_statement
                <|> unparsed_statement parse_statement

            parser input

        parse_statement_ref := parse_statement_implementation
        parse_statement

    /// Parses the rest of statements.
    /// It should be used inside blocks with no constraints, e.g. in unknown statement blocks.
    let parse_many_statements<'a> : Parser<Statement list, 'a> =
        many (spaces >>. parse_statement .>> spaces)

    /// Parses a description statement
    let parse_description_statement<'a> : Parser<DescriptionStatement, 'a> =
        // [RFC 7950, p. 186]
        // description-stmt    = description-keyword sep string stmtend
        skipStringCI "description" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a reference statement
    let parse_namespace_statement<'a> : Parser<NamespaceStatement, 'a> =
        // [RFC 7950, p. 186]
        //namespace-stmt      = namespace-keyword sep uri-str stmtend
        //uri-str             = < a string that matches the rule >
        //                      < URI in RFC 3986 >
        skipStringCI "namespace" >>. spaces >>.
        (Strings.parse_string |>> Uri) .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a prefix statement
    let parse_prefix_statement<'a> : Parser<PrefixStatement, 'a> =
        // [RFC 7950, p. 208]
        //prefix-stmt         = prefix-keyword sep prefix-arg-str stmtend
        //prefix-arg-str      = < a string that matches the rule >
        //                      < prefix-arg >
        //prefix-arg          = prefix
        //prefix              = identifier
        skipStringCI "prefix" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a reference statement
    let parse_reference_statement<'a> : Parser<ReferenceStatement, 'a> =
        // [RFC 7950, p. 186]
        // reference-stmt      = reference-keyword sep string stmtend
        skipStringCI "reference" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a YANG version information
    let parse_yang_version_statement<'a> : Parser<YangVersionStatement, 'a> =
        // [RFC 7950, p. 185]
        //yang-version-stmt   = yang-version-keyword sep yang-version-arg-str
        //                      stmtend
        //yang-version-arg-str = < a string that matches the rule >
        //                          < yang-version-arg >
        //yang-version-arg    = "1.1"        skipStringCI "reference" >>. spaces >>.
        skipStringCI "yang-version" >>. spaces >>.
        (Strings.parse_string |>> Version.Parse) .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Helper method to parse an unknown statement
    let parse_unknown_statement<'a> : Parser<UnknownStatement, 'a> =
        Identifier.parse_identifier_with_prefix
        .>> spaces
        .>>. ((end_of_statement_or_block parse_statement
                |>> (fun body            -> None, body))
                <|>
                (read_keyword .>>. end_of_statement_or_block parse_statement
                |>> (fun (argument, body)    -> Some argument, body))
                )
        |>> (fun (identifier, (argument, body)) -> identifier, argument, body)

