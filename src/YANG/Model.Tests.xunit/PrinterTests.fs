namespace Yang.Model.Tests

module PrinterTests =
    open Xunit
    open Yang.Model.Printer

    [<Theory>]
    [<InlineData("test")>]
    [<InlineData("123")>]
    [<InlineData("123.4")>]
    [<InlineData("/ns1/ns2")>]
    [<InlineData("yang:date-and-time")>]
    let ``simple strings should be unquoted`` (input) =
        let output = ToYangString input
        Assert.Equal(input, output)

    [<Theory>]
    [<InlineData(@"\*")>]
    let ``simple regular expressions should be single-quoted`` (input) =
        let output = ToYangString input
        Assert.True(output.StartsWith("'"))
        Assert.True(output.EndsWith("'"))
        Assert.Equal(input, output.Substring(1, input.Length))

    [<Theory>]
    [<InlineData(@"this is a test")>]
    [<InlineData(@"  test")>]
    [<InlineData(@"test ")>]
    [<InlineData("this\tis\ta\ttest")>]
    [<InlineData("with\nnew\nline")>]
    let ``strings with whitespace should be single-quoted`` (input) =
        let output = ToYangString input
        Assert.True(output.StartsWith("'"))
        Assert.True(output.EndsWith("'"))
        Assert.Equal(input, output.Substring(1, input.Length))

    [<Theory>]
    [<InlineData("test'")>]
    [<InlineData("test'string")>]
    [<InlineData("test'string\nin'new line")>]
    let ``strings with single-quote should be double quoted`` (input) =
        let output = ToYangString input
        Assert.True(output.StartsWith("\""))
        Assert.True(output.EndsWith("\""))
        Assert.Equal(input, output.Substring(1, input.Length))

    [<Fact>]
    let ``string with mixed single and double quote`` () =
        let input = "This' is a \" test of 'a string"
        let expected = """ "This' is a \" test of 'a string" """.Trim()
        let output = ToYangString input
        Assert.True(output.StartsWith("\""))
        Assert.True(output.EndsWith("\""))
        Assert.Equal(expected, output)

