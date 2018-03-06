namespace Yang.Parser.Tests

module IdentifierTests =
    open System
    open Xunit
    open Yang.Parser
    open Yang.Parser.Errors
    open Yang.Parser.Identifier
    open Yang.Model

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
    let ``parse invalid identifier names should fail`` (identifier) =
        Assert.ThrowsAny<Exception>(
            fun _ -> FParsecHelper.apply Identifier.parse_identifier identifier |> ignore
        )

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

    [<Theory>]
    [<InlineData("id1")>]
    let ``parse simple identifier references`` (identifier : string) =
        let id = FParsecHelper.apply Identifier.parse_identifier_reference identifier
        Assert.Equal(Identifier.Simple (Identifier.Make identifier), id)

    [<Theory>]
    [<InlineData("ns", "id")>]
    let ``parse custom identifier references`` (prefix : string, identifier : string) =
        let str = sprintf "%s:%s" prefix identifier
        let id = FParsecHelper.apply Identifier.parse_identifier_reference str
        Assert.Equal(Identifier.Custom (IdentifierWithPrefix.Make (prefix, identifier)), id)
