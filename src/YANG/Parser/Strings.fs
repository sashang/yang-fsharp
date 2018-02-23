// Strings.fs
// Parser for the various types of strings that may appear in a YANG model

namespace Yang.Parser

module Strings =
    open System.Text
    open FParsec

    // Parsing strings: strings can be unquoted, single-quoted, or double-quoted
    // [RFC 7950, Section 6.1.3] defines those as follows (notice the complexity of
    // parsing double-quoted strings w.r.t. removing space for indentation):
    //
    // (the following are copied from the RFC):
    //
    // An unquoted string is any sequence of characters that does not
    // contain any space, tab, carriage return, or line feed characters, a
    // single or double quote character, a semicolon (";"), braces ("{" or
    // "}"), or comment sequences ("//", "/*", or "*/").
    //
    //
    // A single-quoted string (enclosed within ' ') preserves each character
    // within the quotes.  A single quote character cannot occur in a
    // single-quoted string, even when preceded by a backslash.
    //
    //
    // Within a double-quoted string (enclosed within " "), a backslash
    // character introduces a representation of a special character, which
    // depends on the character that immediately follows the backslash:
    //
    // \n      newline
    // \t      a tab character
    // \"      a double quote
    // \\      a single backslash
    //
    // The backslash MUST NOT be followed by any other character.
    //
    // If a quoted string is followed by a plus character ("+"), followed by
    // another quoted string, the two strings are concatenated into one
    // string, allowing multiple concatenations to build one string.
    // Whitespace, line breaks, and comments are allowed between the quoted
    // strings and the plus character.
    //
    // In double-quoted strings, whitespace trimming is done before
    // substitution of backslash-escaped characters.  Concatenation is
    // performed as the last step.
    //
    // If a double-quoted string contains a line break followed by space or
    // tab characters that are used to indent the text according to the
    // layout in the YANG file, this leading whitespace is stripped from the
    // string, up to and including the column of the starting double quote
    // character, or to the first non-whitespace character, whichever occurs
    // first.  Any tab character in a succeeding line that must be examined
    // for stripping is first converted into 8 space characters.

    /// Parses an unquoted string
    let parse_unquoted_string<'a> : Parser<string, 'a> =
        regex "[^ \t\r\n'\";{}]*"
        |> resultSatisfies (
            // Observe that we also need to guarantee that we are not returning the empty string
            fun s -> System.String.IsNullOrWhiteSpace(s) = false &&
                     s.Contains("//") = false && s.Contains("/*") = false && s.Contains("*/") = false
        ) "Invalid unquoted string"

    /// Parses a single quoted string
    let parse_single_quoted_string<'a> : Parser<string, 'a> =
        // Keep everything in a pair of quotes
        skipChar '\'' >>. manySatisfy (fun c -> c <> '\'') .>> skipChar '\''

    /// Removes the whitespace at the beginning of each line
    let private trim_whitespace column (input : string) =
        let strings = input.Split('\n')
        let is_whitespace c = (c = ' ' || c = '\t')

        let strings' =
            strings
            |> Array.map (
                fun line ->
                    if System.String.IsNullOrWhiteSpace(line) then line
                    else
                        let mutable index = 0
                        while is_whitespace (line.Chars index) do
                            index <- index + 1

                        let characters_to_trim = min index column
                        line.Substring(characters_to_trim)
            )
        System.String.Join("\n", strings')

    /// Replaces the escaped control characters in the string with the real values
    let private substitute_control_characters (input : string) =
        input.Replace(@"\\", @"\")
             .Replace(@"\n", "\n")
             .Replace(@"\t", "\t")
             .Replace("\\\"", "\"")

    /// This is a helper structure for capturing the state of the reader when
    /// inside a double quoted string; we need that to identity special characters.
    type private DoubleQuoteParserState =
    | NoEscape
    | Escape
    | EscapeError

    /// Extracts the string inside the the double quotes, as it appears
    let private parse_double_quoted_string_atom_pass1<'a> : Parser<string * bool, 'a> =
        let mutable state = NoEscape
        // We need to keep track of whether this is a multiline string, since
        // we should not remove leading whitespace for non-multiline strings
        let mutable multiline = false

        /// Determine whether the current character is part of the string
        let advance c =
            match c, state with
            // Escape sequence
            | '\\', NoEscape    -> state <- Escape; true
            // End of string
            | '"',  NoEscape    -> false
            // Keep track whether we found a string that spans multiple lines
            | '\n', NoEscape    -> multiline <- true; true
            // Ordinary character in string
            | _,    NoEscape    -> true
            // Input is \\
            | '\\', Escape
            // Input is \n
            | 'n',  Escape
            // Input is \t
            | 't',  Escape
            // Input is \"
            | '"',  Escape
                                -> state <- NoEscape; true
            // Invalid escape character
            | _,    Escape      -> state <- EscapeError; false
            // We have already reached an erroneous escape character; why are we still parsing?
            | _,    EscapeError ->
                raise (new YangParserException("Internal error: parsing of string after determining an invalid special character"))

        manySatisfy advance
        |> resultSatisfies (fun _ -> state <> EscapeError) "Detected invalid control character in string"
        |>> (fun str -> (str, multiline))

    /// Parses a double quoted string
    let parse_double_quoted_string<'a> : Parser<string, 'a> =
        // Parser extracts text from "text", without any formatting
        let parser = skipChar '"' >>. parse_double_quoted_string_atom_pass1 .>> skipChar '"'
        fun stream ->
            let state = stream.State

            // This is the position of the quoted string
            let position = state.GetPosition(stream)

            let reply =
                (
                    parser
                    |>> fun (str, multiline) -> if multiline then trim_whitespace (int position.Column) str else str
                    |>> substitute_control_characters
                ) stream
            if reply.Status = Ok then
                reply
            else
                stream.BacktrackTo(state) // backtrack to beginning
                reply

    /// Parser that interprets the next token as string
    let parse_string<'a> : Parser<string, 'a> =
        // Either there is a single unquoted string, or
        // one or more quoted string concatenated with '+'

        // Parser for quoted strings
        let element : Parser<string, 'a> = parse_single_quoted_string <|> parse_double_quoted_string

        // Next parser for separator.
        // Even though we do not need the string of the separator, we need to consume it,
        // otherwise we cannot parse multi-line text (I think)
        let separator : Parser<string, 'a> = regex "\s+\+\s+"

        // The following three accumulate the parsed strings
        let start (str : string) =
            let sb = StringBuilder()
            sb.Append(str)
        let append (sb : StringBuilder) _ (str : string) = sb.Append(str)
        let finish (sb : StringBuilder) =
            sb.ToString()

        // Parses quoted strings that may be concatenated with '+'
        let parse_quoted_strings : Parser<string, 'a> =
            Inline.SepBy (
                elementParser = element,
                separatorParser = separator,
                stateFromFirstElement = start,
                foldState = append,
                resultFromState = finish
            )

        // Finally, we either have an unquoted string, or one or more quoted strings
        parse_unquoted_string <|> parse_quoted_strings
