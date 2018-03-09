namespace Yang.Parser.Tests

module GenericTests =
    open System
    open Xunit
    open Yang.Parser
    open Yang.Parser.Errors
    open Yang.Parser.Generic

    [<Fact>]
    let ``parse three generic statements`` () =
        let code = """
keyword1;
keyword2;
keyword3;
"""

        let statements = FParsecHelper.apply parse_many_statements code
        Assert.Equal(3, List.length statements)

        match statements with
        | st1 :: st2 :: st3 :: [] ->
            Assert.Equal("keyword1", st1.Keyword)
            Assert.Equal(None, st1.Argument)
            Assert.Equal(None, st1.Body)
            Assert.Equal("keyword2", st2.Keyword)
            Assert.Equal(None, st2.Argument)
            Assert.Equal(None, st2.Body)
            Assert.Equal("keyword3", st3.Keyword)
            Assert.Equal(None, st3.Argument)
            Assert.Equal(None, st3.Body)
        | _ -> failwith "Internal error: testing case should not have reached this point"

    [<Fact>]
    let ``parse simple body with simple statements`` () =
        let code = """
    yang-version 1.1;
    namespace "urn:example:system";
    prefix "sys";

    organization "Example Inc.";
    contact "joe@example.com";
    description
        "The module for entities implementing the Example system.";
"""

        let statements = FParsecHelper.apply parse_many_statements code

        Assert.Equal(6, List.length statements)
        match statements with
        | st1 :: st2 :: st3 :: st4 :: st5 :: st6 :: [] ->
            Assert.Equal("yang-version", st1.Keyword)
            Assert.Equal(Some "1.1", st1.Argument)
            Assert.Equal(None, st1.Body)

            Assert.Equal("namespace", st2.Keyword)
            Assert.Equal(Some "urn:example:system", st2.Argument)
            Assert.Equal(None, st2.Body)

            Assert.Equal("prefix", st3.Keyword)
            Assert.Equal(Some "sys", st3.Argument)
            Assert.Equal(None, st3.Body)

            Assert.Equal("organization", st4.Keyword)
            Assert.Equal(Some "Example Inc.", st4.Argument)
            Assert.Equal(None, st4.Body)

            Assert.Equal("contact", st5.Keyword)
            Assert.Equal(Some "joe@example.com", st5.Argument)
            Assert.Equal(None, st5.Body)

            Assert.Equal("description", st6.Keyword)
            Assert.Equal(Some "The module for entities implementing the Example system.", st6.Argument)
            Assert.Equal(None, st6.Body)
        | _ ->
            failwith "Internal error: testing case should not have reached this point"

    [<Fact>]
    let ``parse simple body with one nested statement`` () =
        let code ="""
keyword {
    yang-version 1.1;
}
"""

        let statements =  FParsecHelper.apply parse_many_statements code
        Assert.Equal(1, List.length statements)
        match statements with
        | statement :: [] ->
            Assert.Equal("keyword", statement.Keyword)
            Assert.Equal(None, statement.Argument)
            Assert.True(statement.Body.IsSome)

            match statement.Body with
            | Some body ->
                Assert.Equal(1, List.length body)

                match body with
                | st :: [] ->
                    Assert.Equal("yang-version", st.Keyword)
                    Assert.Equal(Some "1.1", st.Argument)
                    Assert.Equal(None, st.Body)
                | _ -> failwith "Internal error: testing case should not have reached this point"
            | None -> failwith "Internal error: testing case should not have reached this point"
        | _ ->
            failwith "Internal error: testing case should not have reached this point"

    [<Fact>]
    let ``parse body with nested statements`` () =
        let code = """
keyword {
    yang-version 1.1;
    namespace "urn:example:system";
    prefix "sys";

    organization "Example Inc.";
    contact "joe@example.com";
    description
        "The module for entities implementing the Example system.";
}
"""

        let statements =  FParsecHelper.apply parse_many_statements code
        Assert.Equal(1, List.length statements)

        match statements with
        | statement :: [] ->
            Assert.Equal("keyword", statement.Keyword)
            Assert.Equal(None, statement.Argument)
            Assert.True(statement.Body.IsSome)

            match statement.Body with
            | Some body ->
                Assert.Equal(6, List.length body)
            | None -> failwith "Internal error: testing case should not have reached this point"
        | _ -> failwith "Internal error: testing case should not have reached this point"


    [<Fact>]
    let ``parse body with argument and nested statements`` () =
        let code = """
keyword argument {
    yang-version 1.1;
    namespace "urn:example:system";
    prefix "sys";

    organization "Example Inc.";
    contact "joe@example.com";
    description
        "The module for entities implementing the Example system.";
}
"""

        let statements = FParsecHelper.apply parse_many_statements code
        Assert.Equal(1, List.length statements)

        match statements with
        | statement :: [] ->
            Assert.Equal("keyword", statement.Keyword)
            Assert.Equal(Some "argument", statement.Argument)
            Assert.True(statement.Body.IsSome)

            match statement.Body with
            | Some body ->
                Assert.Equal(6, List.length body)
            | None -> failwith "Internal error: testing case should not have reached this point"
        | _ -> failwith "Internal error: testing case should not have reached this point"

    [<Fact>]
    let ``fail on parsing un-escaped regular expression`` () =
        let input = """pattern "\*";"""
        Assert.ThrowsAny<Exception>(
            fun _ -> FParsecHelper.apply statement_parser input |> ignore
        )

    [<Fact>]
    let ``recover regular expression from correctly encoded RE string`` () =
        let input = """pattern "\\*";"""
        let expected = @"\*"
        let result = FParsecHelper.apply statement_parser input
        Assert.Equal("pattern", result.Keyword)
        Assert.Equal(Some expected, result.Argument)
        Assert.Equal(None, result.Body)

    [<Fact>]
    let ``recover regular expression from correctly encoded single-quoted RE string; RFC 7950, p. 150, example 1`` () =
        let input = """pattern '[a-zA-Z_][a-zA-Z0-9\-_.]*';"""
        let expected = """[a-zA-Z_][a-zA-Z0-9\-_.]*"""
        let result = FParsecHelper.apply statement_parser input
        Assert.Equal("pattern", result.Keyword)
        Assert.Equal(Some expected, result.Argument)
        Assert.Equal(None, result.Body)

    [<Fact>]
    let ``recover regular expression from correctly encoded single-quoted RE string; RFC 7950, p. 150, example 2`` () =
        let input = """pattern '[xX][mM][lL].*' {
         modifier invert-match;
       }"""

        let expected = """[xX][mM][lL].*"""
        let result = FParsecHelper.apply statement_parser input
        Assert.Equal("pattern", result.Keyword)
        Assert.Equal(Some expected, result.Argument)
        Assert.True(result.Body.IsSome)
        Assert.Equal(1, List.length (result.Body.Value))

    [<Fact>]
    let ``parse path statement with multiline string`` () =
        let input = """path
        '/dot1q:bridges'+
        '/dot1q:bridge'+
        '/dot1q:component'+
        '/psfp:flow-meters'+
        '/psfp:flow-meter-instance-table'+
        '/psfp:flow-meter-instance-id';"""

        let expected = @"/dot1q:bridges/dot1q:bridge/dot1q:component/psfp:flow-meters/psfp:flow-meter-instance-table/psfp:flow-meter-instance-id"
        let result = FParsecHelper.apply statement_parser input
        Assert.Equal("path", result.Keyword)
        Assert.Equal(Some expected, result.Argument)
        Assert.Equal(None, result.Body)
