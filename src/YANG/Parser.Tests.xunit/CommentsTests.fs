namespace Yang.Parser.Tests

/// Testing code for comment removal methods
module CommentsTests =
    open System.IO
    open System.Text
    open Xunit
    open Yang.Parser.Comments

    [<Fact>]
    let ``single line comment`` () =
        let line = "// This is a comment"
        let output = Comments.Remove line
        Assert.True(System.String.IsNullOrWhiteSpace(output), "The entire input is a comment and should have been removed")

    [<Fact>]
    let ``single line comment C style`` () =
        let line = "/* This is a comment */"
        let output = Comments.Remove line
        Assert.True(System.String.IsNullOrWhiteSpace(output), "The entire input is a comment and should have been removed")

    [<Fact>]
    let ``single line comment with leading text`` () =
        let line_start = "module test "
        let line = line_start + "// this is a test module"
        let output = Comments.Remove line
        Assert.Equal(line_start, output)

    [<Fact>]
    let ``single line C-style comment with leading text`` () =
        let line_start = "module test "
        let line = line_start + "/* this is a test module */"
        let output = Comments.Remove line
        Assert.Equal(line_start, output)

    [<Fact>]
    let ``comment characters inside string`` () =
        let line = """This is a "//simple" example"""
        let output = Comments.Remove line
        Assert.Equal(line, output)

    [<Fact>]
    let ``block comment characters inside string`` () =
        let line = """This is a "very /* simple */" example"""
        let output = Comments.Remove line
        Assert.Equal(line, output)

    [<Fact>]
    let ``multiline text with single line comment`` () =
        let input = """this is
// a block comment and
a line"""
        let expected = """this is
a line"""
        let output = Comments.Remove input
        Assert.Equal(expected, output)

    [<Fact>]
    let ``multiline text with block comment`` () =
        let input = """this is
/* a block comment and
 * another line comment and
 */a line"""
        let expected = """this is
a line"""
        let output = Comments.Remove input
        Assert.Equal(expected, output)

    [<Fact>]
    let ``empty line afer double comment`` () =
        let input = """module ieee802-dot1ax {
  //
  augment "/if:interfaces/if:interface" {
    when "if:type = 'ianaif:ieee8023adLag'"
  }
}"""
        let output = Comments.Remove input
        let lines = output.Split([| '\n' |])
        Assert.Equal(5, lines.Length)
