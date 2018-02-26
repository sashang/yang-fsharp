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
