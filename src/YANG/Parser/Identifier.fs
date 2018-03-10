// Identifier.fs
// Types and parsers for YANG identifiers.

namespace Yang.Parser

/// Parsers and types for YANG identifiers
module Identifier =
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

    // RFC 7950: Definition of identifier (page 208)
    // identifier          = (ALPHA / "_")
    //                       *(ALPHA / DIGIT / "_" / "-" / ".")
    //
    // Notes:
    //
    // Observe that unquoted strings with invalid characters may parse correctly here,
    // E.g. the input 'example$' recognizes as the identifier name the string 'example',
    // and the invalid character '$' is the next thing to parse. Parsing will fail after
    // as expected. We do not want to add a check for whitespace or eof at the parser
    // (e.g. by adding (spaces1 <|> eof) at the end) because the identifier may be
    // followed by '{' or ';' and we do not want to consume those characters.

    let private isAsciiIdStart c = isAsciiLetter c || c = '_'
    let private isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_' || c = '-' || c = '.'
    let private options = IdentifierOptions(isAsciiIdStart     = isAsciiIdStart,
                                            isAsciiIdContinue = isAsciiIdContinue)
    /// parser for YANG identifier tokens
    let private yang_identifier_element<'a> : Parser<string, 'a> = identifier options

    /// Checks whether a string is a valid identifier name
    let is_identifier_valid (input : string) =
        System.String.IsNullOrWhiteSpace(input) = false &&
        isAsciiIdStart (input.Chars 0) &&
        (String.forall isAsciiIdContinue input)

    /// <summary>
    /// Parses the following token as an identifier
    /// </summary>
    let parse_identifier<'a> : Parser<Identifier, 'a> =
        yang_identifier_element |>> Identifier.MakeUnchecked

    /// Parses the following token as an identifier with prefix (e.g. 'ns:identifier')
    let parse_identifier_with_prefix<'a> : Parser<IdentifierWithPrefix, 'a> =
        yang_identifier_element .>>
        skipChar ':' .>>.
        yang_identifier_element |>> IdentifierWithPrefix.MakeUnchecked

    /// Parses a reference to an identifier. The identifier can be either
    /// common or with prefix
    let parse_identifier_reference<'a> : Parser<IdentifierReference, 'a> =
        yang_identifier_element .>>. (opt (skipChar ':' >>. yang_identifier_element))
        |>> (fun (name1, name2) ->
            match name2 with
            | None -> Simple (Identifier.MakeUnchecked name1)
            | Some name -> Custom (IdentifierWithPrefix.MakeUnchecked (name1, name))
        )

    /// Helper methods for using regular expressions with identifiers
    module RegularExpressions =
        /// Regular expression for normal identifier
        let identifier = "[A-Za-z_][A-Za-z_\-\.0-9]*"

        /// Regular expression for identifier with prefix
        let prefix_identifier_re = sprintf "%s:%s" identifier identifier

        /// Construct a regular expression that matches any of a set of keywords,
        /// or an identifier with prefix
        let keyword_or_prefix_identifier (keywords : string list) =
            let keywords' = String.concat "|" keywords
            sprintf "(%s|%s)($|\s)" keywords' prefix_identifier_re

        /// Parser that captures the next keyword (from the set of input keywords),
        /// or an identifier with prefix
        let parse_next_keyword_or_prefix_identifier (keywords : string list) =
            let re = keyword_or_prefix_identifier keywords
            regex re

        let notFollowedBy_keyword_or_prefix_identifier (keywords : string list) =
            notFollowedBy (parse_next_keyword_or_prefix_identifier keywords)

