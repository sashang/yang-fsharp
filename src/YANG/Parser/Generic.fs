// Generic.fs

namespace Yang.Parser

/// Parsers for generic (i.e. uninterpreted) elements
module Generic =
    open FParsec
    open System.Reflection

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

    // Parsing generic statements
    // Statements are described in [RFC 7950, Section 6.3] as:
    // statement = keyword [argument] (";" / "{" *statement "}")
    //
    // The argument is a string.

    type Statement = {
        Keyword : string
        Argument : string option
        Body: (Statement list) option
    }

    //let (parse_statement : Parser<Statement, 'a>), (parse_statement_ref : Parser<Statement, 'a> ref) =
    let (parse_statement : Parser<Statement, 'a>), (parse_statement_ref : Parser<Statement, 'a> ref) =
        createParserForwardedToRef<Statement, 'a>()

    let parse_statement_implementation<'a> (input : CharStream<'a>) : Reply<Statement> =
        // Below are the conditions for parsing the keyword which is either
        // an identifier or an identifier with prefix. This is why the code
        // below is slightly different from the one used in the Identifier parser.
        let isAsciiIdStart c = isAsciiLetter c || c = '_'
        let isAsciiIdContinue c =
            isAsciiLetter c || isDigit c || c = '_' || c = '-' || c = '.' || c = ':'

        let parser =
            identifier (IdentifierOptions(isAsciiIdStart     = isAsciiIdStart,
                                            isAsciiIdContinue = isAsciiIdContinue))
            .>> spaces
            .>>. (     (skipChar ';' |>> (fun _ -> None, None))
                   <|> (skipChar '{' >>. (manyTill parse_statement (skipChar '}'))
                        |>> (fun body -> None, Some body))
                   <|> (Strings.parse_string .>> spaces .>> skipChar ';'
                        |>> (fun argument -> Some argument, None))
                   <|> (Strings.parse_string .>> spaces .>> skipChar '{' .>>. (manyTill parse_statement (skipChar '}'))
                        |>> (fun (argument, body) -> Some argument, Some body))
                 )
            |>> ( fun (keyword, (argument, body)) ->
                {
                    Keyword     = keyword
                    Argument    = argument
                    Body        = body
                }
            )

        parser input

    do
        parse_statement_ref := parse_statement_implementation
