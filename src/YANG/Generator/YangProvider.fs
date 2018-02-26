namespace Yang.Generator

open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

exception GenericYangProviderError of string

[<TypeProvider>]
type YangProvider (config: TypeProviderConfig) =
    inherit TypeProviderForNamespaces()

    let asm = (Assembly.LoadFrom(config.RuntimeAssembly))
    let schema = makeType asm "Yang"
    let provAsm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))

    member __.X = "F#"
