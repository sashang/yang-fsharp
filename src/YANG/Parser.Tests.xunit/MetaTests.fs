namespace Yang.Parser.Tests

module MetaTests =
    open Xunit
    open FParsec
    open Yang.Parser
    open Yang.Parser.Meta
    open Yang.Model.Statements

    [<Fact>]
    let ``parse empty meta`` () =
        let body = ""
        let meta = FParsecHelper.apply (spaces >>. parse_meta) body
        Assert.Equal<MetaStatements>(MetaStatements.Empty, meta)

    [<Fact>]
    let ``parse simple meta statements`` () =
        let body = """organization "Example Inc.";
    contact "joe@example.com";
    description
        "The module for entities implementing the Example system.";
"""
        let meta = FParsecHelper.apply (spaces >>. parse_meta) body
        let _x = MetaStatements.Organization meta
        Assert.Equal(Some ("Example Inc.", None),       MetaStatements.Organization meta)
        Assert.Equal(Some ("joe@example.com", None),    MetaStatements.Contact meta)
        Assert.Equal(Some ("The module for entities implementing the Example system.", None),
                     MetaStatements.Description meta)
        Assert.Equal(None, MetaStatements.Reference meta)
        Assert.Equal(None, MetaStatements.Unknown   meta)

    // TODO: Add unit test for meta reference
    // TODO: Add unit test for meta unknown
    // TODO: Add unit test for meta organization with extension
    // TODO: Add unit test for meta contact with extension
    // TODO: Add unit test for meta description with extension
    // TODO: Add unit test for meta reference with extension
    // TODO: Add unit test for meta unknown with extension

