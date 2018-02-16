namespace Yang.Parser.Tests

module StringsTests =
    open System
    open Xunit
    open Yang.Parser.Strings

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
