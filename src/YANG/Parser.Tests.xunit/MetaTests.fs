namespace Yang.Parser.Tests

module MetaTests =
    open Xunit
    open FParsec
    open Yang.Parser
    open Yang.Parser.Meta

    [<Fact>]
    let ``parse empty meta`` () =
        let body = ""
        let meta = FParsecHelper.apply (spaces >>. parse_meta) body
        Assert.Null(meta)

    [<Fact>]
    let ``parse simple meta statements`` () =
        let body = """organization "Example Inc.";
    contact "joe@example.com";
    description
        "The module for entities implementing the Example system.";
"""
        let meta = FParsecHelper.apply (spaces >>. parse_meta) body

        Assert.Equal(Some ("Example Inc.", None),       meta.Organization)
        Assert.Equal(Some ("joe@example.com", None),    meta.Contact)
        Assert.Equal(Some ("The module for entities implementing the Example system.", None),
                     meta.Description)
        Assert.Equal(None, meta.Reference)
        Assert.Equal(None, meta.Options)
