namespace Yang.Parser

/// Helper utilities for parsing
[<AutoOpen>]
module Utilities =
    open FParsec

    /// Apply the parser on the input, and return the result.
    /// Fail with exception, if parsing failed.
    let apply_parser parser input =
        match (run parser input) with
        | Success (result, _, _)    -> result
        | Failure (message, _, _)   -> failwith (sprintf "Parsing failed: %s" message)

    /// Checks the result of a parser, and backtracks if token is not valid
    let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
        let error = messageError msg
        fun stream ->
          let state = stream.State
          let reply = p stream
          if reply.Status <> Ok || predicate reply.Result then reply
          else
              stream.BacktrackTo(state) // backtrack to beginning
              Reply(Error, error)

    /// Operator to aid the debugging of parsers
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply

    /// Parser-in-parser: read a string with a string parser, and apply a second
    /// parser on the string read.
    let pip<'a, 'b> (outer : Parser<string, 'a>) (inner : Parser<'b, 'a>) =
        // TODO: Proper testing of parser-in-parser
        // TODO: Do we need to backtrace in parser-in-parser when failure to parse? If so, where?
        // TODO: Make sure that the inside parser in pip consumes the entire input provided by the first parser.
        fun (stream : CharStream<'a>)->
            let state = stream.State
            let input = outer stream
            if input.Status = Ok then
                let str = input.Result
                let cs  = new CharStream<'a>(str, 0, str.Length)
                let output = inner cs
                if output.Status = Ok then
                    Reply output.Result
                else
                    stream.BacktrackTo(state)
                    Reply(Error, output.Error)
            else
                stream.BacktrackTo(state)
                Reply (Error, input.Error)

    /// try_parse_sequence parser1 parser2: Apply parser1 then parser2. If either fails backtrack the stream.
    /// Both of the parsers will be tried, and the combined parser will succeed if either of parser1 or parser2
    /// succeeds.
    let try_parse_sequence<'a, 'T1, 'T2> (parser1 : Parser<'T1, 'a>) (parser2 : Parser<'T2, 'a>) : Parser<('T1 option) * ('T2 option), 'a> =
        fun stream ->
            let state = stream.State
            let reply1 = parser1 stream
            if reply1.Status = Ok then
                let state' = stream.State
                let reply2 = parser2 stream
                if reply2.Status = Ok then
                    let result = Some reply1.Result, Some reply2.Result
                    Reply result
                else
                    stream.BacktrackTo(state')
                    let result = Some reply1.Result, None
                    Reply result
            else
                stream.BacktrackTo(state)
                let reply2 = parser2 stream
                if reply2.Status = Ok then
                    let result = None, Some reply2.Result
                    Reply result
                else
                    stream.BacktrackTo(state)
                    Reply(Error, messageError "Neither parser managed to make progress")

    /// Helper methods on top of FParsec
    type ParserHelper =
        /// <summary>
        /// Applies a parser until it fails, and accumulates the state.
        /// It is similar to Intrinsic.Many, but it will not fail if the element parser fails.
        /// </summary>
        /// <param name="stateFromFirstElement">Construct starting state from first element</param>
        /// <param name="foldState">Update state based on element parsed</param>
        /// <param name="resultFromState">Constructs the final state to return at the end</param>
        /// <param name="elementParser">Parser of elements</param>
        /// <param name="firstElementParser">Parser for first element; if not defined, use the element parser</param>
        /// <param name="resultForEmptySequence">
        ///     Result from empty sequence;
        ///     if not defined use the default value for the output structure
        ///</param>
        static member inline ConsumeMany (  stateFromFirstElement: ('T -> 'State),
                                            foldState: ('State -> 'T -> 'State),
                                            resultFromState: ('State -> 'Result),
                                            elementParser: Parser<'T,'U>,
                                            ?firstElementParser: Parser<'T,'U>,
                                            ?resultForEmptySequence: (unit -> 'Result)
                                         ) : Parser<'Result,'U> =
            let firstElementParser = defaultArg firstElementParser elementParser
            let resultForEmptySequence = defaultArg resultForEmptySequence (fun _ -> Unchecked.defaultof<'Result>)

            fun stream ->
                let state = stream.State
                let reply = firstElementParser stream

                if reply.Status <> Ok then
                    stream.BacktrackTo(state)
                    Reply(resultForEmptySequence ())
                else
                    let mutable processingState : 'State = stateFromFirstElement reply.Result
                    let mutable processing = true

                    while processing do
                        let state' = stream.State
                        let reply' = elementParser stream

                        if reply'.Status <> Ok then
                            stream.BacktrackTo(state')
                            processing <- false
                        else
                            processingState <- foldState processingState reply'.Result

                    Reply(resultFromState processingState)
