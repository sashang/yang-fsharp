namespace Yang.Parser.Tests

open Xunit

[<Collection("Yang Parser")>]
module TypesTests =
    open System
    open Xunit
    open Yang.Model
    open Yang.Parser.Types

    [<Fact>]
    let ``parse simple string type`` () =
        let input = """type string;"""
        let (TypeStatement (id, restriction)) = FParsecHelper.apply parse_type_statement input
        Assert.Equal(IdentifierReference.Make "string", id)
        Assert.Equal(None, restriction)

    [<Fact>]
    let ``parse string type with empty body`` ()=
        let input = """type string {}"""
        let (TypeStatement (id, restriction)) = FParsecHelper.apply parse_type_statement input
        Assert.Equal(IdentifierReference.Make "string", id)
        Assert.True(restriction.IsSome)
        let string_restrictions = TypeBodyStatement.AsStringRestrictions restriction.Value
        Assert.True(string_restrictions.IsSome)
        Assert.Equal(0, string_restrictions.Value.Length)

    [<Fact>]
    let ``parse string type with simple length constraint`` () =
        let input = """type string {
                 length "1 .. 128";
               }"""
        let (TypeStatement (id, restriction)) = FParsecHelper.apply parse_type_statement input
        Assert.Equal(IdentifierReference.Make "string", id)
        Assert.NotEqual(None, restriction)

    [<Fact>]
    let ``parse string type with custom constraints`` () =
        let input = """type string {
                 junos:posix-pattern "^.{1,64}$";
                 junos:pattern-message "Must be string of 64 characters or less";
               }"""
        let (TypeStatement (id, restriction)) = FParsecHelper.apply parse_type_statement input
        Assert.Equal(Identifier.IdentifierReference.Make "string", id)
        Assert.True(restriction.IsSome)
        let string_restriction = TypeBodyStatement.AsStringRestrictions restriction.Value
        Assert.True(string_restriction.IsSome)
        Assert.Equal(2, string_restriction.Value.Length)
        // TODO: Check that the string restrictions are unknown

    [<Fact>]
    let ``type string with type restrictions appear before custom extensions`` () =
        let input = """type string {
             length "1 .. 128";
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
           }"""

        let (TypeStatement (id, restriction)) = FParsecHelper.apply parse_type_statement input
        Assert.Equal(Identifier.IdentifierReference.Make "string", id)
        Assert.True(restriction.IsSome)
        let string_restriction = TypeBodyStatement.AsStringRestrictions restriction.Value
        Assert.True(string_restriction.IsSome)
        Assert.Equal(3, string_restriction.Value.Length)
        // TODO: Check that the string restrictions are correct

    [<Fact>]
    let ``parse string type with custom extensions and restrictions`` () =
        let input = """type string {
                 junos:posix-pattern "^.{1,64}$";
                 junos:pattern-message "Must be string of 64 characters or less";
                 length "1 .. 128";
               }"""
        let (TypeStatement (id, restriction)) = FParsecHelper.apply parse_type_statement input
        Assert.Equal(Identifier.IdentifierReference.Make "string", id)
        Assert.True(restriction.IsSome)
        let string_restriction = TypeBodyStatement.AsStringRestrictions restriction.Value
        Assert.True(string_restriction.IsSome)
        Assert.Equal(3, string_restriction.Value.Length)
        // TODO: Check that the string restrictions are correct

    [<Fact>]
    let ``parse string type with both length and pattern restriction`` () =
        let input = """type string {
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
             length "1 .. 64";
             pattern "^.{1,64}$";
           }"""

        let (TypeStatement (id, restriction)) = FParsecHelper.apply parse_type_statement input
        Assert.Equal(Identifier.IdentifierReference.Make "string", id)
        Assert.True(restriction.IsSome)
        let string_restriction = TypeBodyStatement.AsStringRestrictions restriction.Value
        Assert.True(string_restriction.IsSome)
        Assert.Equal(4, string_restriction.Value.Length)
        // TODO: Check that the string restrictions are correct

    [<Fact>]
    let ``parse string with three pattern restrictions, length, and unknowns`` () =
        let input = """type string {
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
             length "1 .. 64";
             pattern "^.{1,62}$";
             pattern "^.{1,64}$";
             pattern "^.{1,60}$";
           }"""

        let (TypeStatement (id, restriction)) = FParsecHelper.apply parse_type_statement input
        Assert.Equal(Identifier.IdentifierReference.Make "string", id)
        Assert.True(restriction.IsSome)
        let string_restriction = TypeBodyStatement.AsStringRestrictions restriction.Value
        Assert.True(string_restriction.IsSome)
        Assert.Equal(6, string_restriction.Value.Length)
        // TODO: Check that the string restrictions are correct

    [<Fact>]
    let ``parse simple type statement`` () =
        let input = """type performance-15min-interval {
}"""
        let (TypeStatement (id, body)) = FParsecHelper.apply parse_type_statement input
        Assert.Equal("performance-15min-interval", id.Value)
        Assert.True(id.IsValid)
        Assert.True(id._IsSimple)

    [<Theory>]
    [<InlineData("string7only")>]
    [<InlineData("string-huge")>]
    let ``parse custom type with keyword prefix`` (name) =
        let input = sprintf "type %s;" name
        let (TypeStatement (id, extra)) = FParsecHelper.apply parse_type_statement input
        Assert.True(id.IsValid)
        Assert.True(extra.IsNone)
        Assert.Equal(name, id.Value)

    [<Theory>]
    [<InlineData("string")>]
    [<InlineData("string-huge")>]
    [<InlineData("assoc-type")>]
    let ``parse types in strings`` (name) =
        let input1 = sprintf "type \"%s\";" name
        let input2 = sprintf "type '%s';"   name

        let (TypeStatement (id1, extra1)) = FParsecHelper.apply parse_type_statement input1
        let (TypeStatement (id2, extra2)) = FParsecHelper.apply parse_type_statement input2
        Assert.True(id1.IsValid)
        Assert.True(id2.IsValid)
        Assert.True(extra1.IsNone)
        Assert.True(extra2.IsNone)
        Assert.Equal(name, id1.Value)
        Assert.Equal(name, id2.Value)
