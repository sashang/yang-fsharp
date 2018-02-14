// Identifier.fs
// Types and parsers for YANG identifiers.

namespace Yang.Parser

/// Parsers and types for YANG identifiers
module Identifier =
    open FParsec

    // RFC 7950: Definition of identifier (page 208)
    // identifier          = (ALPHA / "_")
    //                       *(ALPHA / DIGIT / "_" / "-" / ".")

    let private isAsciiIdStart c = isAsciiLetter c || c = '_'
    let private isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_' || c = '-' || c = '.'

    /// YANG Identifier
    [<StructuredFormatDisplay("{Value}")>]
    type Identifier = private | String of string
    with
        static member private CheckValid (input : string) =
            System.String.IsNullOrWhiteSpace(input) = false &&
            isAsciiIdStart (input.Chars 0) &&
            (String.forall isAsciiIdContinue input)

        /// <summary>
        /// Creates an identifier from the input string,
        /// without checking whether the string is valid.
        /// The caller should guarantee validity of the input.
        /// </summary>
        /// <param name="input">The identifier</param>
        static member MakeUnchecked (input : string) = String input

        /// <summary>
        /// Creates an identifier from the input string,
        /// </summary>
        /// <param name="input">The input identifier</param>
        static member Make (input : string) =
            if (Identifier.CheckValid input) = false then
                raise (new YangParserException(sprintf "Invalid identifier: %s" input))
            else
                String input

        /// <summary>
        /// Gets the string value of the identifier
        /// </summary>
        member this.Value = let (String value) = this in value

        /// <summary>
        /// Checks whether the identifier has a valid name
        /// </summary>
        member this.IsValid = Identifier.CheckValid this.Value

        override this.ToString() = this.Value

    /// <summary>
    /// Parses the following token as an identifier
    /// </summary>
    let parse_identifier<'a> : Parser<Identifier, 'a> =
        identifier (IdentifierOptions(isAsciiIdStart     = isAsciiIdStart,
                                       isAsciiIdContinue = isAsciiIdContinue))
        |>> Identifier.MakeUnchecked
