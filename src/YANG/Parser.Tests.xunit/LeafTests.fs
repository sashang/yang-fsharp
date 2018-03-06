namespace Yang.Parser.Tests

module LeafTests =
    open Xunit
    open Yang.Parser
    open Yang.Model

    [<Fact>]
    let ``parse leaf type with string`` () =
        let body = """leaf host-name {
    type string;
    description
        "Hostname for this system.";
}"""

        let leaf = FParsecHelper.apply Leaf.parse_leaf body

        Assert.Equal("host-name", LeafStatement.IdentifierAsString leaf)
        Assert.Equal(
            LeafBodyStatement.Type (IdentifierReference.Make "string", None, None) |> Option.Some,
            LeafStatement.Type leaf
        )

        Assert.Equal(
            LeafBodyStatement.Description ("Hostname for this system.", None) |> Option.Some,
            LeafStatement.Description leaf
        )

    // TODO: Add unit tests for more types of leafs and more properties
