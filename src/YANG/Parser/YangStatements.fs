// YangStatements.fs
namespace Yang.Parser

/// Parsers for common statements, that appear in the yang-stmt rule
[<AutoOpen>]
module YangStatements =
    open System
    open FParsec
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
    | Contact of Contact:string * Options:(Statement list option)
    | Description of Description:string * Options:(Statement list option)
    | Namespace of Namespace:string * Options:(Statement list option)
    | Organization of Organization:string * Options:(Statement list option)
    | Prefix of Prefix:string * Options:(Statement list option)
    | YangVersion of Version:Version * Options:(Statement list option)
    | Unknown of Identifier:IdentifierWithPrefix * Argument:(string option) * Body:(Statement list option)
    | Unparsed of Identifier:Identifier * Argument:(string option) * Body:(Statement list option)
    with
        member this.Identifier =
            match this with
            | Contact _         -> "contact"
            | Description _     -> "description"
            | Namespace _       -> "namespace"
            | Organization _    -> "organization"
            | Prefix _          -> "prefix"
            | YangVersion _     -> "yang-version"
            | Unknown (id, _, _)    -> id.ToString()
            | Unparsed (id, _, _)   -> id.ToString()

        member this.PrettyPrint =
            let escape = [| ' '; '\t'; '\r'; '\n'; ';'; '{'; '}'; '@' |]

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
            | Namespace (s, b)
            | Organization (s, b)
            | Prefix (s, b)
                -> sprintf "%s %s %s" id (ps s) (pb b)
            | YangVersion (version, block)  -> sprintf "%s %s %s" id (version.ToString()) (pb block)
            | Unknown (id, arg, body)       -> sprintf "Unknown: %A %s %s"  id (pso arg) (pb body)
            | Unparsed (id, arg, body)      -> sprintf "Unparsed: %A %s %s" id (pso arg) (pb body)


    // Helper definitions that consume trailing whitespace
    let inline read_keyword<'a> : Parser<string, 'a> = Strings.parse_string .>> spaces
    let inline end_of_statement<'a> : Parser<unit, 'a> = skipChar ';' >>. spaces
    let inline begin_block<'a> : Parser<unit, 'a> = skipChar '{' .>> spaces
    let inline end_block<'a> : Parser<unit, 'a> = spaces .>> skipChar '}' .>> spaces

    let inline block<'a> (parser : Parser<Statement, 'a>) : Parser<Statement list, 'a> =
        manyTill parser end_block

    let inline end_of_statement_or_block<'a> (parser : Parser<Statement, 'a>) : Parser<Statement list option, 'a> =
        (end_of_statement |>> (fun _ -> None))
        <|>
        (begin_block >>. (block parser) |>> (fun statements -> Some statements))

    let inline unknown_statement<'a> (parser : Parser<Statement, 'a>) : Parser<Statement, 'a> =
            Identifier.parse_identifier_with_prefix
            .>> spaces
            .>>. ((end_of_statement_or_block parser
                   |>> (fun body            -> None, body))
                  <|>
                  (read_keyword .>>. end_of_statement_or_block parser
                   |>> (fun (argument, body)    -> Some argument, body))
                 )
            |>> (fun (identifier, (argument, body)) -> Unknown (identifier, argument, body))

    let inline unparsed_statement<'a> (parser : Parser<Statement, 'a>) : Parser<Statement, 'a> =
            Identifier.parse_identifier
            .>> spaces
            .>>. ((end_of_statement_or_block parser
                   |>> (fun body            -> None, body))
                  <|>
                  (read_keyword .>>. end_of_statement_or_block parser
                   |>> (fun (argument, body)    -> Some argument, body))
                 )
            |>> (fun (identifier, (argument, body)) -> Unparsed (identifier, argument, body))

    let inline yang_keywowrd_string_statement<'a> (keyword : string, maker) (parser : Parser<Statement, 'a>) : Parser<Statement, 'a> =
        skipStringCI keyword >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parser
        |>> maker

    let inline parse_statement<'a> : Parser<Statement, 'a> =
        let (parse_statement : Parser<Statement, 'a>), (parse_statement_ref : Parser<Statement, 'a> ref) =
            createParserForwardedToRef<Statement, 'a>()

        let inline parse_statement_implementation (input : CharStream<'a>) : Reply<Statement> =
            let parser =
                    yang_keywowrd_string_statement (
                        "yang-version",
                        (fun (version, options) ->
                            let version' = Version.Parse version
                            YangVersion (version', options)
                        )) parse_statement
                <|> yang_keywowrd_string_statement ("contact", Contact) parse_statement
                <|> yang_keywowrd_string_statement ("description", Description) parse_statement
                <|> yang_keywowrd_string_statement ("namespace", Namespace) parse_statement
                <|> yang_keywowrd_string_statement ("organization", Organization) parse_statement
                <|> yang_keywowrd_string_statement ("prefix", Prefix) parse_statement
                <|> unknown_statement parse_statement
                <|> unparsed_statement parse_statement

            parser input

        parse_statement_ref := parse_statement_implementation
        parse_statement

    /// Parses the rest of statements.
    let parse_many_statements<'a> : Parser<Statement list, 'a> =
        many (spaces >>. parse_statement .>> spaces)
