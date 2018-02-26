namespace Yang.Parser.Tests

module HeaderTests =
    open System
    open Xunit
    open FParsec
    open Yang.Parser
    open Yang.Parser.Header

    [<Fact>]
    let ``parse simple header`` () =
        let body = """yang-version 1.1;
    namespace "urn:example:system";
    prefix "sys";
"""

        let header = FParsecHelper.apply (spaces >>. parse_header) body
        Assert.NotNull(header)
        Assert.Equal((Version (1, 1), None),            header.YangVersion)
        Assert.Equal((Uri("urn:example:system"), None), header.Namespace)
        Assert.Equal(("sys", None),                     header.Prefix)
        Assert.Equal(None,                              header.Options)
