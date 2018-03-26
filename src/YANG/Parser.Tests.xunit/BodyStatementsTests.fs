namespace Yang.Parser.Tests

open Xunit

[<Collection("Yang Parser")>]
module DataDefinitionsTests =
    open Xunit
    open FParsec
    open Yang.Model
    open Yang.Parser
    open Yang.Parser.BodyStatements
    open Yang.Parser.Types

    [<Fact>]
    let ``parse case statement`` () =
        let input = """case "non-candidate" {
        leaf non-candidate-bsr-state {
          type enumeration {
            enum "no-info";
            enum "accept-any";
            enum "accept";
          }
        }
      }"""
        let (CaseStatement (id, body)) = FParsecHelper.apply parse_case_statement input
        Assert.True(id.IsValid)
        Assert.Equal("non-candidate", id.Value)
        Assert.True(body.IsSome)
        Assert.Equal(1, body.Value.Length)
        Assert.True(CaseBodyStatement.IsLeaf body.Value.Head)

    [<Fact>]
    let ``parse container statement`` () =
        let input = """container hqosShareShape {
            when "not(../../prioritymodefq='is4cos' or ../../prioritymodefq='ispriority') or ../../prioritymodefq='notpriority'";
            description
              "Share-Shaping.";}"""
        let (ContainerStatement (id, body)) = FParsecHelper.apply parse_container_statement input
        Assert.True(id.IsValid)
        Assert.Equal("hqosShareShape", id.Value)
        Assert.True(body.IsSome)
        Assert.Equal(2, body.Value.Length)

    [<Fact>]
    let ``parse container statement with long name`` () =
        let input = """container "optical-logical-interface-logical-channel"+
          "-assignments" {
          description
            "The operational attributes for a particular
            interface";
}"""
        let (ContainerStatement (id, body)) = FParsecHelper.apply parse_container_statement input
        Assert.True(id.IsValid)
        Assert.Equal("optical-logical-interface-logical-channel-assignments", id.Value)
        Assert.True(body.IsSome)
        Assert.Equal(1, body.Value.Length)

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
        | Container (ContainerStatement (id, body)) ->
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

    [<Fact>]
    let ``parse grouping statement simple`` () =
        let input = """grouping mpls-rsvp-session-state {
    list session {
      key "source-port destination-port
       source-address destination-address";
    }
  }"""
        let (GroupingStatement (id, body)) = FParsecHelper.apply parse_grouping_statement input
        Assert.True(id.IsValid)
        Assert.Equal("mpls-rsvp-session-state", id.Value)
        Assert.True(body.IsSome)
        Assert.Equal(1, body.Value.Length)

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
    let ``parse list statement with long name`` () =
        let input = """list "optical-logical-interface-logical-channel"+
            "-assignment" {
            key "index";

            container "optical-logical-interface-logical-channel"+
              "-assignment-attr" {
              uses LOGICAL-CHANNEL-ASSIGNMENT;
            }
            leaf index {
              type int32;
            }
          }"""
        let (ListStatement (id, body)) = FParsecHelper.apply parse_list_statement input
        Assert.True(id.IsValid)
        Assert.Equal("optical-logical-interface-logical-channel-assignment", id.Value)
        Assert.Equal(3, body.Length)

    [<Fact>]
    let ``parse typedef statement simple`` () =
        let input = """typedef performance-15min-history-interval {
type performance-15min-interval {
}
}"""
        let (TypeDefStatement (id, body)) = FParsecHelper.apply parse_typedef_statement input
        Assert.Equal("performance-15min-history-interval", id.Value)
        Assert.True(id.IsValid)
        Assert.Equal(1, body.Length)

    // TODO: More extensive unit testing of BodyStatements

    [<Fact>]
    let ``parse unknown statement simple`` () =
        let input = """smiv2:alias "ciscoQosPIBMIB" {
    smiv2:oid "1.3.6.1.4.1.9.18.2.1";
  }"""
        let statement = FParsecHelper.apply parse_body_statement input
        Assert.True(BodyStatement.IsUnknown statement)
        let unknown = BodyStatement.AsUnknown statement
        Assert.True(unknown.IsSome)
        let (UnknownStatement (id, label, body)) = unknown.Value
        Assert.True(id.IsValid)
        Assert.True(label.IsSome)
        Assert.Equal("ciscoQosPIBMIB", label.Value)
        Assert.True(body.IsSome)
        Assert.Equal(1, body.Value.Length)
