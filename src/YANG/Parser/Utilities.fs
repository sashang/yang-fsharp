namespace Yang.Parser

/// Helper utilities for parsing
[<AutoOpen>]
module Utilities =
    open FParsec

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

    type ParserHelper =
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
