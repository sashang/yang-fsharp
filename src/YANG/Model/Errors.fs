// Errors.fs
// Definition of exceptions from the Model library

namespace Yang.Model

[<AutoOpen>]
module Errors =
    open System

    type YangModelException (message:string, ?innerException:exn) =
        inherit ApplicationException(
            message,
            match innerException with | Some ex -> ex | _ -> null)