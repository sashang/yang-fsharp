// Generic.fs

namespace Yang.Parser

/// Parsers for generic (i.e. uninterpreted) elements
module Generic =
    open FParsec

    /// Tracks the state of a generic parser
    type private TextState =
    /// The parser is in normal state, i.e. not in a string
    | Normal
    /// The parser is in a single-quoted string
    | SingleQuotedString
    /// The parser is in a double-quoted string
    | DoubleQuotedString
    /// The parser is in a double-quoted string and the previous character was an escape,
    /// hence a double quote is not the end of the string.
    | Escaped

    /// <summary>
    /// Parses a block as a string. The block starts with '{',
    /// ends with '}', can contain a number of nested sub-blocks
    /// (which should be paired), and can contain arbitrary characters
    /// inside single- and double- quoted strings.
    /// </summary>
    let unparsed_block_literal<'a> : Parser<string, 'a> =
        let mutable opened = 0
        let mutable state : TextState = Normal

        /// Updates the state of the parser based on the current character
        let closing c : bool =
            match state, c with
            // Exiting single quoted string
            | SingleQuotedString, '\''  -> state <- Normal; true
            // Keep parsing single quoted string
            | SingleQuotedString, _     -> true
            // Exiting double quoted string
            | DoubleQuotedString, '"'   -> state <- Normal; true
            // Entering escape character
            | DoubleQuotedString, '\\'  -> state <- Escaped; true
            // Keep parsing double quoted string
            | DoubleQuotedString, _     -> true
            // Exit escape character
            | Escaped, _                -> state <- DoubleQuotedString; true
            // Enter single quoted string
            | Normal, '\''              -> state <- SingleQuotedString; true
            // Enter double quoted string
            | Normal, '"'               -> state <- DoubleQuotedString; true
            // Beginning of a new block (sub-block)
            | Normal, '{'               -> opened <- opened + 1; true
            // End of a block
            | Normal, '}'               ->
                if opened = 0 then
                    // End of the main block
                    false
                else
                    // End of sub-block
                    opened <- opened - 1
                    true
            // Continue parsing
            | Normal, _else             -> true

        let normalChar = satisfy closing
        skipChar '{' >>. manyChars normalChar .>> skipChar '}'

