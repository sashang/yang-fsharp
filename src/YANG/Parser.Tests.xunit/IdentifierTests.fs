namespace Yang.Parser.Tests

module IdentifierTests =
    open Xunit
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
    [<InlineData("2018")>]
    [<InlineData(".check")>]
    [<InlineData("")>]
    [<InlineData(null)>]
    [<InlineData("my$name")>]
    let ``check invalid identifier names`` (identifier) =
        let id = Identifier.MakeUnchecked identifier
        Assert.False(id.IsValid)

    [<Fact>]
    let ``invalid identifier should throw exception`` () =
        Assert.Throws<YangParserException>(fun _ -> Identifier.Make ".invalid" |> ignore)
