namespace Yang.XmlHelper

[<AutoOpen>]
module Errors =
    open System

    type XmlHelperException (message:string, ?innerException:exn) =
        inherit ApplicationException(
            message,
            match innerException with | Some ex -> ex | _ -> null)

    type XmlParsingException (message : string, ?innerException:exn) =
        inherit XmlHelperException(
            message,
            match innerException with | Some ex -> ex | _ -> null)

