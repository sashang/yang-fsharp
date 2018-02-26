namespace Yang.Parser.Tests

module LeafTests =
    open Xunit
    open Yang.Parser

    [<Fact>]
    let ``parse leaf type with string`` () =
        let body = """leaf host-name {
    type string;
    description
        "Hostname for this system.";
}"""

        let definition = FParsecHelper.apply Leaf.parse_leaf body
        Assert.Equal("host-name", definition.Identifier.Value)
        Assert.Equal((Types.TString, None), definition.Type)
        Assert.Equal(Some (Leaf.Description ("Hostname for this system.", None)), definition.Description)
