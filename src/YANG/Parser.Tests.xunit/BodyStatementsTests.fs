﻿namespace Yang.Parser.Tests

module DataDefinitionsTests =
    open Xunit
    open FParsec
    open Yang.Model
    open Yang.Parser
    open Yang.Parser.BodyStatements
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

        let t = FParsecHelper.apply (spaces >>. parse_body_statement) body
        Assert.True(BodyStatement.IsLeaf t)

        match t with
        | Leaf leaf ->
            Assert.Equal("host-name", LeafStatement.IdentifierAsString leaf)
            let statements = LeafStatement.Statements leaf
            Assert.Equal(2, statements.Length)

            match statements with
            | ts :: ds :: [] ->
                // We want the statements to appear in the same order as they are in the input
                Assert.True(LeafBodyStatement.IsType ts)
                Assert.True(LeafBodyStatement.IsDescription ds)
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

        let t = FParsecHelper.apply (spaces >>. parse_body_statement) body
        Assert.True(BodyStatement.IsLeafList t)

        match t with
        | LeafList ll ->
            Assert.Equal("domain-search", LeafListStatement.IdentifierAsString ll)
            let statements = LeafListStatement.Statements ll
            Assert.Equal(2, statements.Length)

            match statements with
            | ts :: ds :: [] ->
                Assert.True(LeafListBodyStatement.IsType ts)
                Assert.True(LeafListBodyStatement.IsDescription ds)
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
        let t = FParsecHelper.apply (spaces >>. parse_body_statement) body
        Assert.True(BodyStatement.IsContainer t)

        match t with
        | Container (id, body) ->
            Assert.Equal("system", id.Value)
            Assert.True(body.IsSome)

            let b = body.Value
            Assert.NotEmpty(b)
            Assert.Equal(3, b.Length)

            match b with
            | l1 :: l2 :: c2 :: [] ->
                Assert.True(ContainerBodyStatement.IsLeaf l1)
                Assert.True(ContainerBodyStatement.IsLeafList l2)
                Assert.True(ContainerBodyStatement.IsContainer c2)

            | _ -> failwith "Internal error: unit test should not have reached this point"
        | _ -> failwith "Internal error: unit test should not have reached this point"

    // TODO: More extensive unit testing of BodyStatements