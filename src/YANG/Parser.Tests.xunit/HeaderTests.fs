namespace Yang.Parser.Tests

module HeaderTests =
    open System
    open Xunit
    open FParsec
    open Yang.Parser
    open Yang.Parser.Header
    open Yang.Model

    [<Fact>]
    let ``parse simple header`` () =
        let body = """yang-version 1.1;
    namespace "urn:example:system";
    prefix "sys";
"""

        let version, ns, prefix, other = FParsecHelper.apply (spaces >>. parse_header) body
        Assert.Equal(YangVersionStatement (Version (1, 1), None),           version)
        Assert.Equal(NamespaceStatement (Uri("urn:example:system"), None),  ns)
        Assert.Equal(PrefixStatement ("sys", None),                         prefix)
        Assert.Equal(None,                                                  other)

    // TODO: Add unit test for module header with extensions
    // TODO: Add unit test for module header with missing version
    // TODO: Add unit test for module header with missing namespace
    // TODO: Add unit test for module header with missing prefix
