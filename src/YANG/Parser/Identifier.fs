// Identifier.fs
// Types and parsers for YANG identifiers.

namespace Yang.Parser

/// Parsers and types for YANG identifiers
module Identifier =
    open FParsec
    open NLog

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

    /// Checks whether a string is a valid identifier name
    let is_identifier_valid (input : string) =
        System.String.IsNullOrWhiteSpace(input) = false &&
        isAsciiIdStart (input.Chars 0) &&
        (String.forall isAsciiIdContinue input)

    /// YANG Identifier
    [<StructuredFormatDisplay("{Value}")>]
    type Identifier = private | String of string
    with
        /// <summary>
        /// Creates an identifier from the input string,
        /// without checking whether the string is valid.
        /// The caller should guarantee validity of the input.
        /// </summary>
        /// <param name="name">The identifier</param>
        static member MakeUnchecked (name : string) = String name

        /// <summary>
        /// Creates an identifier from the input string,
        /// </summary>
        /// <param name="name">The input identifier</param>
        static member Make (name : string) =
            if (is_identifier_valid name) = false then
                _logger.Error(sprintf "Invalid identifier: %s" name)
                raise (new YangParserException(sprintf "Invalid identifier: %s" name))
            else
                String name

        /// <summary>
        /// Gets the string value of the identifier
        /// </summary>
        member this.Value = let (String value) = this in value

        /// <summary>
        /// Checks whether the identifier has a valid name
        /// </summary>
        member this.IsValid = is_identifier_valid this.Value

        override this.ToString() = this.Value

    /// <summary>
    /// Parses the following token as an identifier
    /// </summary>
    let parse_identifier<'a> : Parser<Identifier, 'a> =
        identifier (IdentifierOptions(isAsciiIdStart     = isAsciiIdStart,
                                       isAsciiIdContinue = isAsciiIdContinue))
        |>> Identifier.MakeUnchecked

    /// YANG Identifier with prefix
    [<StructuredFormatDisplay("{Value}")>]
    type IdentifierWithPrefix = {
        Prefix  : string
        Name    : string
    }
    with
        /// <summary>
        /// Creates a composite identifier without checking validity of input string;
        /// Caller should guarantee that the prefix and name are valid.
        /// </summary>
        /// <param name="prefix">The prefix of the identifier</param>
        /// <param name="name">The name of the identifier</param>
        static member MakeUnchecked (prefix, name) = { Prefix = prefix; Name = name }

        /// <summary>
        /// Creates a composite identifier
        /// </summary>
        /// <param name="prefix">The prefix of the identifier</param>
        /// <param name="name">The name of the identifier</param>
        static member Make (prefix, name) =
            if (is_identifier_valid prefix) = false then
                _logger.Error(sprintf "Invalid prefix for identifier: %s" prefix)
                raise (new YangParserException(sprintf "Invalid prefix for identifier: %s" prefix))
            if (is_identifier_valid name) = false then
                _logger.Error(sprintf "Invalid name of (prefixed) identifier: %s" name)
                raise (new YangParserException(sprintf "Invalid name of (prefixed) identifier : %s" name))
            { Prefix = prefix; Name = name }

        /// <summary>
        /// Gets the string value of the identifier
        /// </summary>
        member this.Value = sprintf "%s:%s" this.Prefix this.Name

        /// <summary>
        /// Checks whether the identifier has a valid name
        /// </summary>
        member this.IsValid = (is_identifier_valid this.Prefix) && (is_identifier_valid this.Name)

        override this.ToString() = this.Value

    /// Parses the following token as an identifier with prefix (e.g. 'ns:identifier')
    let parse_identifier_with_prefix<'a> : Parser<IdentifierWithPrefix, 'a> =
        identifier (IdentifierOptions(isAsciiIdStart     = isAsciiIdStart,
                                       isAsciiIdContinue = isAsciiIdContinue)) .>>
        skipChar ':' .>>.
        identifier (IdentifierOptions(isAsciiIdStart     = isAsciiIdStart,
                                       isAsciiIdContinue = isAsciiIdContinue))
        |>> IdentifierWithPrefix.MakeUnchecked
