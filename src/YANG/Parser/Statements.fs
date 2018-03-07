// Statements.fs
namespace Yang.Parser

/// Parsers for common statements, that appear in the yang-stmt rule
[<AutoOpen>]
module Statements =
    open System
    open FParsec
    open Identifier
    open Yang.Model

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
            |>> (fun (identifier, (argument, body)) -> Statement.Unknown (identifier, argument, body))

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
        skipString keyword >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parser
        |>> maker

    let inline private yang_keyword_uri_statement<'a> (keyword : string, maker) (parser : Parser<Statement, 'a>) : Parser<Statement, 'a> =
        skipString keyword >>. spaces >>.
        (Strings.parse_string |>> Uri) .>> spaces .>>.
        end_of_statement_or_block parser
        |>> maker

    /// Parses a yang-stmt [RFC 7950, p. 202].
    /// It should be used for parsing rules with no constraints, e.g.
    // inside unknown-statement rules.
    let inline parse_statement<'a> : Parser<Statement, 'a> =
        // This parser accepts any type of statement. Typically, it should be used for statements
        // in unknown statements that have no constraints. Because, of their generality they can
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
                <|> yang_keyword_string_statement ("contact", Statement.Contact)            parse_statement
                <|> yang_keyword_string_statement ("description", Statement.Description)    parse_statement
                <|> yang_keyword_string_statement ("error-app-tag", Statement.ErrorMessage) parse_statement
                <|> yang_keyword_string_statement ("error-message", Statement.ErrorMessage) parse_statement
                <|> yang_keyword_uri_statement    ("namespace", Statement.Namespace)        parse_statement
                <|> yang_keyword_string_statement ("organization", Statement.Organization)  parse_statement
                <|> yang_keyword_string_statement ("prefix", Statement.Prefix)              parse_statement
                <|> yang_keyword_string_statement ("presence", Statement.Presence)          parse_statement
                <|> yang_keyword_string_statement ("reference", Statement.Reference)        parse_statement
                <|> unknown_statement parse_statement
                <|> unparsed_statement parse_statement

            parser input

        parse_statement_ref := parse_statement_implementation
        parse_statement

    /// Parses the rest of statements.
    /// It should be used inside blocks with no constraints, e.g. in unknown statement blocks.
    let parse_many_statements<'a> : Parser<Statement list, 'a> =
        many (spaces >>. parse_statement .>> spaces)

    /// Parses a config statement
    let parse_config_statement<'a> : Parser<ConfigStatement, 'a> =
        // [RFC 7950, p. 191]
        //config-stmt         = config-keyword sep
        //                        config-arg-str stmtend
        //config-arg-str      = < a string that matches the rule >
        //                        < config-arg >
        //config-arg          = true-keyword / false-keyword
        skipString "config" >>. spaces >>.
        Arguments.parse_boolean .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a contact statement
    let parse_contact_statement<'a> : Parser<ContactStatement, 'a> =
        // [RFC 7950, p. 186]
        // contact-stmt        = contact-keyword sep string stmtend
        skipString "contact" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a description statement
    let parse_description_statement<'a> : Parser<DescriptionStatement, 'a> =
        // [RFC 7950, p. 186]
        // description-stmt    = description-keyword sep string stmtend
        skipString "description" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses an error message statement
    let parse_error_app_tag_statement<'a> : Parser<ErrorAppTagStatement, 'a> =
        // [RFC 7950, p. 192]
        // error-app-tag-stmt  = error-app-tag-keyword sep string stmtend
        skipString "error-app-tag" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses an error message statement
    let parse_error_message_statement<'a> : Parser<ErrorMessageStatement, 'a> =
        // [RFC 7950, p. 192]
        // error-message-stmt  = error-message-keyword sep string stmtend
        skipString "error-message" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a key statement
    let parse_key_statement<'a> : Parser<KeyStatement, 'a> =
        // [RFC 7950, p. 195]
        //key-stmt            = key-keyword sep key-arg-str stmtend
        //key-arg-str         = < a string that matches the rule >
        //                        < key-arg >
        //key-arg             = node-identifier *(sep node-identifier)

        skipString "key" >>. spaces >>.
        Strings.parse_string .>> spaces .>>. 
        (end_of_statement_or_block parse_statement) .>> spaces
        |>> (fun (key, block) -> Arguments.Key.MakeFromString key, block)

    let parse_mandatory_statement<'a> : Parser<MandatoryStatement, 'a> =
        // [RFC 7950, p. 192]
        //mandatory-stmt      = mandatory-keyword sep
        //                        mandatory-arg-str stmtend
        //mandatory-arg-str   = < a string that matches the rule >
        //                        < mandatory-arg >
        //mandatory-arg       = true-keyword / false-keyword
        skipString "mandatory" >>. spaces >>.
        Arguments.parse_boolean .>> spaces .>>.
        (end_of_statement_or_block parse_statement) .>> spaces

    let parse_max_elements_statement<'a> : Parser<MaxElementsStatement, 'a> =
        // [RFC 7950, p. 192]
        //max-elements-stmt   = max-elements-keyword sep
        //                        max-value-arg-str stmtend
        skipString "max-elements" >>. spaces >>.
        Arguments.parse_max_value .>> spaces .>>.
        (end_of_statement_or_block parse_statement) .>> spaces

    /// Parses a reference statement
    let parse_namespace_statement<'a> : Parser<NamespaceStatement, 'a> =
        // [RFC 7950, p. 186]
        //namespace-stmt      = namespace-keyword sep uri-str stmtend
        //uri-str             = < a string that matches the rule >
        //                      < URI in RFC 3986 >
        skipString "namespace" >>. spaces >>.
        (Strings.parse_string |>> Uri) .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    let parse_ordered_by_statement<'a> : Parser<OrderedByStatement, 'a> =
        // [RFC 7950, p. 192]
        //ordered-by-stmt     = ordered-by-keyword sep
        //                        ordered-by-arg-str stmtend
        //ordered-by-arg-str  = < a string that matches the rule >
        //                        < ordered-by-arg >
        //ordered-by-arg      = user-keyword / system-keyword
        skipString "ordered-by" >>. spaces >>.
        (
                (skipString "user"      |>> (fun _ -> Arguments.OrderedBy.User))
            <|> (skipString "system"    |>> (fun _ -> Arguments.OrderedBy.System))
        ) .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses an organization statement
    let parse_organization_statement<'a> : Parser<OrganizationStatement, 'a> =
        // [RFC 7950, p. 186]
        // organization-stmt   = organization-keyword sep string stmtend
        skipString "organization" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a prefix statement
    let parse_prefix_statement<'a> : Parser<PrefixStatement, 'a> =
        // [RFC 7950, p. 208]
        //prefix-stmt         = prefix-keyword sep prefix-arg-str stmtend
        //prefix-arg-str      = < a string that matches the rule >
        //                      < prefix-arg >
        //prefix-arg          = prefix
        //prefix              = identifier
        skipString "prefix" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a presence statement
    let parse_presence_statement<'a> : Parser<PresenceStatement, 'a> =
        // [RFC 7950, p. 192]
        // presence-stmt       = presence-keyword sep string stmtend
        skipString "presence" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a reference statement
    let parse_reference_statement<'a> : Parser<ReferenceStatement, 'a> =
        // [RFC 7950, p. 186]
        // reference-stmt      = reference-keyword sep string stmtend
        skipString "reference" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    let parse_revision_date_statement<'a> : Parser<RevisionDateStatement, 'a> =
        // [RFC 7950, p. 186]
        // revision-date-stmt  = revision-date-keyword sep revision-date stmtend
        // [RFC 7950, p.207]
        // revision-date-keyword    = %s"revision-date"
        skipString "revision-date" >>. spaces >>.
        Arguments.parse_date .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    let parse_status_statement<'a> : Parser<StatusStatement, 'a> =
        // [RFC 7950, p. 191]
        //status-stmt         = status-keyword sep status-arg-str stmtend
        //status-arg-str      = < a string that matches the rule >
        //                        < status-arg >
        //status-arg          = current-keyword /
        //                        obsolete-keyword /
        //                        deprecated-keyword
        skipString "status" >>. spaces >>.
        Arguments.parse_status .>> spaces .>>.
        end_of_statement_or_block parse_statement .>> spaces

    /// Parses a YANG version information
    let parse_yang_version_statement<'a> : Parser<YangVersionStatement, 'a> =
        // [RFC 7950, p. 185]
        //yang-version-stmt   = yang-version-keyword sep yang-version-arg-str
        //                      stmtend
        //yang-version-arg-str = < a string that matches the rule >
        //                          < yang-version-arg >
        //yang-version-arg    = "1.1"        skipString "reference" >>. spaces >>.
        skipString "yang-version" >>. spaces >>.
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

    let parse_length_body_statement<'a> : Parser<LengthBodyStatement, 'a> =
            (parse_error_message_statement  |>> LengthBodyStatement.ErrorMessage)
        <|> (parse_error_app_tag_statement  |>> LengthBodyStatement.ErrorAppTag)
        <|> (parse_description_statement    |>> LengthBodyStatement.Description)
        <|> (parse_reference_statement      |>> LengthBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> LengthBodyStatement.Unknown)

    /// Parses a length statement
    let parse_length_statement<'a> : Parser<LengthStatement, 'a> =
        // [RFC 7950, p. 189]
        //length-stmt         = length-keyword sep length-arg-str optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            [error-message-stmt]
        //                            [error-app-tag-stmt]
        //                            [description-stmt]
        //                            [reference-stmt]
        //                        "}") stmtsep
        skipString "length" >>. spaces >>.
        Arguments.parse_length .>> spaces .>>.
        (
                (skipChar ';' |>> (fun _ -> None))
            <|> (skipChar '{' >>. spaces >>.
                 (many (parse_length_body_statement .>> spaces)) .>> spaces .>>
                 skipChar '}' |>> Some
                )
        ) .>> spaces

    let parse_pattern_body_statement<'a> : Parser<PatternBodyStatement, 'a> =
            (parse_error_message_statement      |>> PatternBodyStatement.ErrorMessage)
        <|> (parse_error_app_tag_statement      |>> PatternBodyStatement.ErrorAppTag)
        <|> (parse_description_statement        |>> PatternBodyStatement.Description)
        <|> (parse_reference_statement          |>> PatternBodyStatement.Reference)
        <|> (parse_unknown_statement            |>> PatternBodyStatement.Unknown)

    let parse_pattern_statement<'a> : Parser<PatternStatement, 'a> =
        // [RFC 7950, p. 190]
        //pattern-stmt        = pattern-keyword sep string optsep
        //                        (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                            [modifier-stmt]
        //                            [error-message-stmt]
        //                            [error-app-tag-stmt]
        //                            [description-stmt]
        //                            [reference-stmt]
        //                        "}") stmtsep
        skipString "pattern" >>. spaces >>.
        Strings.parse_string .>> spaces .>>.
        (
                (skipChar ';'       |>> (fun _ -> None))
            <|> (skipChar '{' >>. spaces >>.
                 (many parse_pattern_body_statement) .>> spaces .>>
                 skipChar '}'
                 |>> Some
                )
        ) .>> spaces
