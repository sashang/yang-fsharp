namespace Yang.Parser.Tests

module StatementsTests =
    open FParsec
    open Xunit
    open Yang.Model
    open Yang.Parser.Statements

    [<Theory>]
    [<InlineData("contact 'Joe Dev'; contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev';contact 'Jim Hacker';;")>]
    [<InlineData("contact 'Joe Dev';;contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev'; ; contact 'Jim Hacker';;")>]
    [<InlineData("contact 'Joe Dev';; contact 'Jim Hacker';;;")>]
    [<InlineData("contact 'Joe Dev'; ;contact 'Jim Hacker';;;")>]
    [<InlineData("contact 'Joe Dev';;;;;;contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev'; ; ; ; ; ; contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev' { } contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev' { }; contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev' { ; } contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev' { ; } ; contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev' { opt:when 'true'; } contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev' { ; opt:when 'true'; } contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev' { opt:when 'true'; ;} contact 'Jim Hacker';")>]
    [<InlineData("contact 'Joe Dev' { ;opt:when 'true'; } contact 'Jim Hacker';")>]
    let ``parse statements with empty statements in between`` (input) =
        let result = FParsecHelper.apply (many parse_contact_statement) input
        Assert.Equal(2, result.Length)
