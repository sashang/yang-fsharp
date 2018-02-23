namespace Yang.Parser.Tests

module StringsTests =
    open System
    open Xunit
    open Yang.Parser.Strings
    open FParsec

    [<Theory>]
    [<InlineData("test")>]
    [<InlineData("test/")>]
    [<InlineData("/")>]
    [<InlineData("/test")>]
    [<InlineData("test/other")>]
    [<InlineData("test*other")>]
    [<InlineData("123")>]
    let ``parse valid unquoted strings`` (input) =
        let str = FParsecHelper.apply parse_unquoted_string input
        Assert.Equal(input, str)

    [<Theory>]
    [<InlineData("test/*")>]
    [<InlineData("test//")>]
    [<InlineData("test//other")>]
    [<InlineData("*/test")>]
    [<InlineData("*/")>]
    [<InlineData("//")>]
    [<InlineData("/*")>]
    [<InlineData("test*/")>]
    [<InlineData("test*/other")>]
    let ``fail with invalid unquoted string`` (input) =
        Assert.ThrowsAny<Exception>(
            fun _ -> FParsecHelper.apply parse_unquoted_string input |> ignore
        )

    [<Theory>]
    [<InlineData("test", "test")>]
    [<InlineData("'test 123 !£$%'", "test 123 !£$%")>]
    [<InlineData("\"string with space\"", "string with space")>]
    [<InlineData("\'string with space\'", "string with space")>]
    [<InlineData("\"Test \\\" string\"", "Test \" string")>]
    [<InlineData("\"string A \" + \"and string B\"", "string A and string B")>]
    [<InlineData("'string A ' + \"and string B\"", "string A and string B")>]
    [<InlineData("'string A ' + 'and string B'", "string A and string B")>]
    let ``parse valid strings`` (input, expected) =
        let str = FParsecHelper.apply parse_string input
        Assert.Equal(expected, str)

    [<Fact>]
    let ``parse multiline string`` () =
        let multiline_string = """
        "the message starts here and
         continues to the next line"
"""
        let expected = """the message starts here and
continues to the next line"""

        let str = FParsecHelper.apply (spaces >>. parse_string) multiline_string
        // Observe that in YANG all line endings are with \n, and not \r\n.
        Assert.Equal(expected.Replace("\r\n", "\n"), str)

    [<Fact>]
    let ``parse multiline string with addition`` () =
        let multiline_string = """
        "the message starts here and " +
        "continues to the next line" + " and a bit more"
"""

        let expected = """the message starts here and continues to the next line and a bit more"""
        let str = FParsecHelper.apply (spaces >>. parse_string) multiline_string
        // Observe that in YANG all line endings are with \n, and not \r\n.
        Assert.Equal(expected.Replace("\r\n", "\n"), str)

    [<Fact>]
    let ``parse multiline string with concatenation`` () =
        let multiline_string = """
        "the message starts here and
         continues to the next line " +
     "and continues here
      and there"
"""

        let expected = """the message starts here and
continues to the next line and continues here
and there"""

        let str = FParsecHelper.apply (spaces >>. parse_string) multiline_string
        // Observe that in YANG all line endings are with \n, and not \r\n.
        Assert.Equal(expected.Replace("\r\n", "\n"), str)
