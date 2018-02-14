// Errors.fs
// Definitions of base exceptions

namespace Yang.Parser

[<AutoOpen>]
module Errors =
    open System

    type YangParserException (message:string, ?innerException:exn) =
        inherit ApplicationException(
            message,
            match innerException with | Some ex -> ex | _ -> null)
