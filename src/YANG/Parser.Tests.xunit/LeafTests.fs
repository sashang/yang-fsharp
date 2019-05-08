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

        let leaf = FParsecHelper.apply Leaf.parse_leaf_statement body

        Assert.Equal("host-name", LeafStatement.IdentifierAsString leaf)
        Assert.Equal(
            LeafBodyStatement.Type (TypeStatement (IdentifierReference.Make "string", None)) |> Option.Some,
            LeafStatement.Type leaf
        )

        Assert.Equal(
            LeafBodyStatement.Description (DescriptionStatement ("Hostname for this system.", None)) |> Option.Some,
            LeafStatement.Description leaf
        )

    [<Fact>]
    let ``parse leaf with double quotes in name`` () =
        let input = """leaf "destination-port" {
        type uint16;
        description
          "RSVP source port";
        reference "RFC 2205";
      }"""
        let (LeafStatement (id, body)) = FParsecHelper.apply Leaf.parse_leaf_statement input
        Assert.True(id.IsValid)
        Assert.Equal("destination-port", id.Value)
        Assert.Equal(3, body.Length)

    // TODO: Add unit tests for more types of leafs and more properties
