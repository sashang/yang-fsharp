namespace Yang.Parser.Tests

open System
open Yang.Parser
open Xunit

// The following is used to initialize the Yang Parser.
// All modules that implement unit tests and that require proper
// initialization of the statement parser must use the attribute:
// [<Collection("Yang Parser")>]

type YangParserInitializationFixture () =
    do Parser.Initialize()

    interface IDisposable with
        member __.Dispose() = ()

[<CollectionDefinition("Yang Parser")>]
type YangParserTestCollection () =
    interface ICollectionFixture<YangParserInitializationFixture>

