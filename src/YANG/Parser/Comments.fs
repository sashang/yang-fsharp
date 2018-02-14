namespace Yang.Parser

/// Methods to remove comments from YANG files
[<AutoOpen>]
module Comments =
    // Pre-processes the input grammar to remove the comments.
    // Removing the comments simplifies parsing later on.

    // From RFC 7950, Section 6.1.1.
    //   Comments are C++ style.  A single line comment starts with "//" and
    //   ends at the end of the line.  A block comment starts with "/*" and
    //   ends with the nearest following "*/".
    //
    //   Note that inside a quoted string (Section 6.1.3), these character
    //   pairs are never interpreted as the start or end of a comment.

    // From RFC 7950, Section 6.1.3
    //    A single-quoted string (enclosed within ' ') preserves each character
    //    within the quotes.  A single quote character cannot occur in a
    //    single-quoted string, even when preceded by a backslash.
    //
    //    Within a double-quoted string (enclosed within " "), a backslash
    //    character introduces a representation of a special character, which
    //    depends on the character that immediately follows the backslash:
    //
    //    \n      newline
    //    \t      a tab character
    //    \"      a double quote
    //    \\      a single backslash
    //
    //    The backslash MUST NOT be followed by any other character.

    // Implementation approach:
    // Due to the complexity of multi-line comments and comment characters inside strings,
    // it has been difficult to find a regular expression to perform the job. Instead, we
    // parse the input character-by-character to remove the inputs.
    //  The approach below may be slow (calling the virtual methods Read(), Peek(), and Write() all the time).
    // However, even in big files (Juniper YANG configuration model -- 7.5MB), it takes around 210ms in a
    // reasonable old machine, including reading the file and writing to text.

    open System
    open System.IO
    open System.Text

    /// State of the parser
    type private State =
    /// Normal parsing
    | Normal
    /// Parser is inside a single quoted string, e.g. 'example string'
    | SingleQuotedString
    /// Parser is inside a double quoted string, e.g. 'example string'
    | DoubleQuotedString
    /// Parser is inside a single line comment, e.g. '// example comment'
    | SingleLineComment
    /// Parser is inside a block comment, which may be multiline, e.g. '/* example comment */'
    | BlockComment

    /// Parser that removes comments from input
    let private SlowRemove (input : TextReader) (output : TextWriter) =
        let rec advance (state : State) =
            let c = input.Read()
            if c = -1 then
                // end of input
                if state = BlockComment then
                    // we have reached the end of file without reading the end of the comment block ("*/")
                    // TODO: write a warning here
                    ()
                ()
            else
                match state with
                | Normal ->
                    if c = int '"' then
                        output.Write(char c)
                        advance DoubleQuotedString
                    elif c = int '\'' then
                        output.Write(char c)
                        advance SingleQuotedString
                    elif c = int '/' then
                        let next_c = input.Peek()
                        if next_c = -1 then
                            // End of input
                            output.Write(char c)
                            ()
                        elif next_c = int '/' then
                            input.Read() |> ignore
                            advance SingleLineComment
                        elif next_c = int '*' then
                            input.Read() |> ignore
                            advance BlockComment
                        else
                            output.Write(char c)
                            advance Normal
                    else
                        output.Write(char c)
                        advance Normal

                | SingleQuotedString ->
                    output.Write(char c)
                    if c = int '\'' then
                        // reached end of single quoted string
                        advance Normal
                    else
                        advance SingleQuotedString

                | DoubleQuotedString ->
                    output.Write(char c)
                    if c = int '"' then
                        // reached end of double quoted string
                        advance Normal
                    elif c = int '\\' then
                        // reached escape character
                        let next_c = input.Read()
                        if next_c = -1 then
                            // nothing follows the escape character; end of file reached
                            // TODO: write a warning here
                            ()
                        else
                            output.Write(char next_c)
                            advance DoubleQuotedString
                    else
                        advance DoubleQuotedString

                | SingleLineComment ->
                    input.ReadLine() |> ignore
                    advance Normal

                | BlockComment ->
                    if c = int '*' then
                        let next_c = input.Peek()
                        if next_c = -1 then
                            // reached end of file without reading the closing block comment ("*/")
                            // TODO: write a warning here
                            ()
                        elif next_c = int '/' then
                            input.Read() |> ignore
                            advance Normal
                        else
                            advance BlockComment
                    else
                        advance BlockComment

        advance Normal

    /// Removes comments from a string, and returns a string
    let private SlowRemoveFromString input =
        use input' = new StringReader(input)
        let sb = StringBuilder ()
        use output = new StringWriter(sb)
        SlowRemove input' output
        let output' = sb.ToString()
        output'

    // The API to remove comments is defined below.
    // We define them in a type to be able to reuse the same method name
    // (by overloading the arguments)

    type Comments =
        /// <summary>
        /// Removes comments from a YANG input; caller provides input and output streams
        /// </summary>
        /// <param name="input">Input stream</param>
        /// <param name="output">Output stream</param>
        static member Remove(input, output) = SlowRemove input output

        /// <summary>
        /// Remove comments from a string input
        /// </summary>
        /// <param name="input">Input model</param>
        /// <returns>Model without comments</returns>
        static member Remove input = SlowRemoveFromString input
