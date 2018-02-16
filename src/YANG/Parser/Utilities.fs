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
