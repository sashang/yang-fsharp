// Strings.fs
// Parser for the various types of strings that may appear in a YANG model

namespace Yang.Parser

module Strings =
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

    let parse_unquoted_string<'a> : Parser<string, 'a> =
        failwith "Not implemented exception"

    let parse_single_quoted_string<'a> : Parser<string, 'a> =
        failwith "Not implemented exception"

    let parse_double_quoted_string<'a> : Parser<string, 'a> =
        failwith "Not implemented exception"

    /// Parser that interprets the next token as string
    let parse_string<'a> : Parser<string, 'a> =
        parse_unquoted_string <|> parse_single_quoted_string <|> parse_double_quoted_string


