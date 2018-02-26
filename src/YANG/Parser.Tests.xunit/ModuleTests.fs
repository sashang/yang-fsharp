namespace Yang.Parser.Tests

module ModuleTests =
    open Xunit
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
        Assert.NotNull(m.Revisions)
        Assert.True(m.Revisions.IsSome)
        Assert.Equal(1, m.Revisions.Value.Length)
        // The rest of the statements are covered by other unit tests
