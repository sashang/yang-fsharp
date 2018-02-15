﻿namespace Yang.Parser.Tests

module IdentifierTests =
    open System
    open Xunit
    open Yang.Parser
    open Yang.Parser.Errors
    open Yang.Parser.Identifier

    [<Theory>]
    [<InlineData("example-system")>]
    [<InlineData("_another")>]
    [<InlineData("_2017")>]
    [<InlineData("_.")>]
    [<InlineData("A.")>]
    [<InlineData("A.B")>]
    let ``check valid identifier names`` (identifier) =
        let id = Identifier.Make identifier
        Assert.True(id.IsValid)

    [<Theory>]
    [<InlineData("example-system")>]
    [<InlineData("_another")>]
    [<InlineData("_2017")>]
    [<InlineData("_.")>]
    [<InlineData("A.")>]
    [<InlineData("A.B")>]
    let ``parse valid identifier names`` (identifier) =
        let id = FParsecHelper.apply Identifier.parse_identifier identifier
        Assert.Equal(identifier, id.Value)

    [<Theory>]
    [<InlineData("2018")>]
    [<InlineData(".check")>]
    [<InlineData("")>]
    [<InlineData(null)>]
    [<InlineData("my$name")>]
    let ``check invalid identifier names`` (identifier) =
        let id = Identifier.MakeUnchecked identifier
        Assert.False(id.IsValid)

    [<Theory>]
    [<InlineData("2018")>]
    [<InlineData(".check")>]
    [<InlineData("")>]
    [<InlineData(null)>]
    let ``parse invalid identifier names should fail`` (identifier) =
        Assert.ThrowsAny<Exception>(
            fun _ -> FParsecHelper.apply Identifier.parse_identifier identifier |> ignore
        )

    [<Fact>]
    let ``invalid identifier should throw exception`` () =
        Assert.Throws<YangParserException>(fun _ -> Identifier.Make ".invalid" |> ignore)

    [<Theory>]
    [<InlineData("ns:example-system")>]
    [<InlineData("ns2:A")>]
    [<InlineData("ns2:A.B")>]
    let ``parse valid identifiers with prefix`` (identifier : string) =
        let expected = identifier.Split(':')
        let id = FParsecHelper.apply Identifier.parse_identifier_with_prefix identifier
        Assert.Equal(2, expected.Length)
        Assert.Equal(expected.[0], id.Prefix)
        Assert.Equal(expected.[1], id.Name)

    [<Theory>]
    [<InlineData(":name")>]
    [<InlineData("ns:")>]
    [<InlineData(":")>]
    [<InlineData("")>]
    [<InlineData(".check:name")>]
    [<InlineData("ns:.name")>]
    let ``invalid prefixed identifiers should fail`` (identifier : string) =
        Assert.ThrowsAny<Exception>(
            fun _ -> FParsecHelper.apply Identifier.parse_identifier_with_prefix identifier |> ignore
        )
