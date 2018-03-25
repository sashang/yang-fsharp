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

        let m = FParsecHelper.apply parse_module (model.Trim())
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
        // TODO: do we want to return null when the header does not exist?
        Assert.Null(m.Header)
        Assert.Equal<LinkageStatements>([], m.Linkage)
        Assert.Equal<MetaStatements>([], m.Meta)
        Assert.Equal<RevisionStatement list>([], m.Revision)
        Assert.NotNull(m.Body)
        Assert.Empty(m.Body)

    // TODO: add more module tests

    [<Fact>]
    let ``parse simple submodule`` () =
        let input = """submodule openconfig-mpls-igp {

  yang-version "1";

  belongs-to "openconfig-mpls" {
    prefix "mpls";
  }
}"""
        let submodule = FParsecHelper.apply parse_submodule input
        Assert.Equal("openconfig-mpls-igp", submodule.Name.Value)
        let (YangVersionStatement (version, yve), (BelongsToStatement (belongs_to, bte)), extra) = submodule.Header
        Assert.Equal(1, version.Major)
        Assert.Equal(0, version.Minor)
        Assert.True(yve.IsNone)

        Assert.Equal("openconfig-mpls", belongs_to.Value)
        Assert.Equal(1, bte.Length)
        let prefix = bte.Head
        Assert.True(BelongsToBodyStatement.IsPrefix prefix)
        let p = BelongsToBodyStatement.AsPrefix prefix
        Assert.True(p.IsSome)
        let (PrefixStatement (pp, pe)) = p.Value
        Assert.Equal("mpls", pp)
        Assert.True(pe.IsNone)
        Assert.Equal(0, submodule.Body.Length)

    // TODO: add tests for submodules
