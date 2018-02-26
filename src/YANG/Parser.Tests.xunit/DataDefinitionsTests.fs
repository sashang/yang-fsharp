namespace Yang.Parser.Tests

module DataDefinitionsTests =
    open Xunit
    open FParsec
    open Yang.Parser
    open Yang.Parser.DataDefinitions
    open Yang.Parser.Types

    [<Fact>]
    let ``parse leaf definition with type string`` () =
        let body = """
        leaf host-name {
            type string;
            description
                "Hostname for this system.";
        }
        """

        let t = FParsecHelper.apply (spaces >>. parse_data_definition) body
        Assert.True(IsLeaf t)

        match t with
        | Leaf leaf ->
            Assert.Equal("host-name", leaf.Identifier.Value)
            Assert.NotEmpty(leaf.Statements)
            Assert.Equal(2, leaf.Statements.Length)

            match leaf.Statements with
            | ts :: ds :: [] ->
                // We want the statements to appear in the same order as they are in the input
                Assert.True(Leaf.IsTypeStatement ts)
                Assert.True(Leaf.IsDescriptionStatement ds)
            | _ -> failwith "Internal error: unit test should not have reached this point"
        | _ -> failwith "Internal error: unit test should not have reached this point"

    [<Fact>]
    let ``parse leaf-list definition with type string`` () =
        let body = """
leaf-list domain-search {
    type string;
    description
        "List of domain names to search.";
}
"""

        let t = FParsecHelper.apply (spaces >>. DataDefinitions.parse_data_definition) body
        Assert.True(IsLeafList t)

        match t with
        | LeafList ll ->
            Assert.Equal("domain-search", ll.Identifier.Value)
            Assert.NotEmpty(ll.Statements)
            Assert.Equal(2, ll.Statements.Length)

            match ll.Statements with
            | ts :: ds :: [] ->
                Assert.True(LeafList.IsTypeStatement ts)
                Assert.True(LeafList.IsDescriptionStatement ds)
            | _ -> failwith "Internal error: unit test should not have reached this point"
        | _ -> failwith "Internal error: unit test should not have reached this point"
