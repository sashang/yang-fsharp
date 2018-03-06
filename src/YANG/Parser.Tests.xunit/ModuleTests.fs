namespace Yang.Parser.Tests

module ModuleTests =
    open System
    open Xunit
    open Yang.Model
    open Yang.Parser
    open Yang.Parser.Module

    [<Fact>]
    let ``parse simple module without definitions`` () =
        let model = """
module example-system {
    yang-version 1.1;
    namespace "urn:example:system";
    prefix "sys";

    organization "Example Inc.";
    contact "joe@example.com";
    description
        "The module for entities implementing the Example system.";

    revision 2007-06-09 {
        description "Initial revision.";
    }
}
"""

        let m = FParsecHelper.apply parse_module model
        Assert.Equal("example-system", m.Name.Value)
        Assert.NotNull(m.Header)
        Assert.NotNull(m.Meta)
        Assert.NotNull(m.Revision)
        Assert.Equal(1, m.Revision.Length)
        // The rest of the statements are covered by other unit tests

    [<Fact>]
    let ``parse empty string as a module should fail`` () =
        Assert.ThrowsAny<Exception>(
            fun _ -> FParsecHelper.apply parse_module "" |> ignore
        )

    [<Fact>]
    let ``parsing module with no namespace should fail`` () =
        let body = """module {
    yang-version 1.1;
    namespace "urn:example:system";
    prefix "sys";
}"""
        Assert.ThrowsAny<Exception>(
            fun _ -> FParsecHelper.apply parse_module "" |> ignore
        )

    [<Fact>]
    let ``parsing module with empty body`` () =
        let m = FParsecHelper.apply parse_module "module name {}"
        Assert.Equal("name", m.Name.Value)
        Assert.Null(m.Header)
        Assert.Equal<LinkageStatements>([], m.Linkage)
        Assert.Equal<MetaStatements>([], m.Meta)
        Assert.Equal<RevisionStatement list>([], m.Revision)
        Assert.NotNull(m.Body)
        Assert.Empty(m.Body)
