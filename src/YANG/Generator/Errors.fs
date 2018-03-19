// Errors.fs
// Definition of exceptions from the Generator library

namespace Yang.Generator

[<AutoOpen>]
module Errors =
    open System

    type YangGeneratorException (message:string, ?innerException:exn) =
        inherit ApplicationException(
            message,
            match innerException with | Some ex -> ex | _ -> null)

    type XmlParsingException (message : string, ?innerException:exn) =
        inherit YangGeneratorException(
            message,
            match innerException with | Some ex -> ex | _ -> null)

    type ProgramCreationException (message : string, ?innerException:exn) =
        inherit YangGeneratorException(
            message,
            match innerException with | Some ex -> ex | _ -> null)
