namespace Yang.Parser.Tests

module LinkageTests =
    open Xunit
    open Yang.Model
    open Yang.Parser.Linkage

    // TODO: add unit tests for linkage import statement

    // TODO: add unit tests for linkage include statement
    let ``parse include statement simple`` () =
        let input = """include "Cisco-IOS-XR-ipv4-ping-act";"""
        let (IncludeStatement (id, extra)) = FParsecHelper.apply parse_include_statement input
        Assert.True(id.IsValid)
        Assert.Equal("Cisco-IOS-XR-ipv4-ping-act", id.Value)
        Assert.True(extra.IsNone)

