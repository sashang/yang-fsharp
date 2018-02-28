namespace Yang.Generator

[<AutoOpen>]
module Types =
    open ProviderImplementation.ProvidedTypes

    let private _logger : (string -> unit) option ref = ref None

    let SetLogger (logger : (string -> unit) option) = _logger := logger

    type private Logger =
        static member Log (message : string) =
            match !_logger with
            | None -> ()
            | Some logger -> logger message

    let ns = "Yang.YangProvider"

    let internal makeType asm typeName =
        ProvidedTypeDefinition( asm, ns, typeName, Some typeof<obj>, IsErased = false)
