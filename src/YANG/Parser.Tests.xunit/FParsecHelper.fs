namespace Yang.Parser.Tests

module FParsecHelper =
    open FParsec

    let apply p str =
        match run p str with
        | Success(result, _, _ )    -> result
        | Failure(errorMsg, _, _)   -> failwith errorMsg

