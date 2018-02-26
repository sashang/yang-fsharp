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

    [<Fact>]
    let ``parse container with embedded container`` () =
        let body ="""
container system {
    leaf host-name {
        type string;
        description
            "Hostname for this system.";
    }

    leaf-list domain-search {
        type string;
        description
            "List of domain names to search.";
    }

    container login {
        leaf message {
            type string;
            description
            "Message given at start of login session.";
        }

        list user {
            key "name";
            leaf name {
                type string;
            }
            leaf full-name {
                type string;
            }
            leaf class {
                type string;
            }
        }
    }
}
"""
        let t = FParsecHelper.apply (spaces >>. DataDefinitions.parse_data_definition) body
        Assert.True(IsContainer t)

        match t with
        | Container (id, body) ->
            Assert.Equal("system", id.Value)
            Assert.True(body.IsSome)

            let b = body.Value
            Assert.NotEmpty(b)
            Assert.Equal(3, b.Length)

            match b with
            | (ContainerStatementBody.DataDefinition l1) :: (ContainerStatementBody.DataDefinition l2) :: (ContainerStatementBody.DataDefinition c2) :: [] ->
                Assert.True(IsLeaf l1)
                Assert.True(IsLeafList l2)
                Assert.True(IsContainer c2)
            // Force fail, if list is not of expected type
            | _ -> Assert.True(false, "Unexpected list of elements in list")
        | _ -> failwith "Internal error: unit test should not have reached this point"
