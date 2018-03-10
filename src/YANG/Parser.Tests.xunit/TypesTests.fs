namespace Yang.Parser.Tests

module TypesTests =
    open System
    open Xunit
    open Yang.Model
    open Yang.Parser.Types

    [<Fact>]
    let ``parse simple string type`` () =
        let input = """type string;"""
        let id, restriction, unknowns = FParsecHelper.apply parse_type_statement input
        Assert.Equal(IdentifierReference.Make "string", id)
        Assert.Equal(None, restriction)
        Assert.Equal(None, unknowns)

    [<Fact>]
    let ``parse string type with empty body`` ()=
        let input = """type string {}"""
        let id, restriction, unknowns = FParsecHelper.apply parse_type_statement input
        Assert.Equal(IdentifierReference.Make "string", id)
        Assert.Equal(None, restriction)
        Assert.Equal(None, unknowns)

    [<Fact>]
    let ``parse string type with simple length constraint`` () =
        let input = """type string {
                 length "1 .. 128";
               }"""
        let id, restriction, unknowns = FParsecHelper.apply parse_type_statement input
        Assert.Equal(IdentifierReference.Make "string", id)
        Assert.NotEqual(None, restriction)
        Assert.Equal(None, unknowns)

    [<Fact>]
    let ``parse string type with custom constraints`` () =
        let input = """type string {
                 junos:posix-pattern "^.{1,64}$";
                 junos:pattern-message "Must be string of 64 characters or less";
               }"""
        let id, restriction, unknowns = FParsecHelper.apply parse_type_statement input
        Assert.Equal(Identifier.IdentifierReference.Make "string", id)
        Assert.Equal(None, restriction)
        Assert.NotEqual(None, unknowns)
        Assert.Equal(2, unknowns.Value.Length)

    [<Fact>]
    let ``type string with type restrictions appear before custom extensions`` () =
        let input = """type string {
             length "1 .. 128";
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
           }"""

        let id, restriction, unknowns = FParsecHelper.apply parse_type_statement input
        Assert.Equal(Identifier.IdentifierReference.Make "string", id)
        Assert.NotEqual(None, restriction)
        Assert.NotEqual(None, unknowns)
        Assert.Equal(2, unknowns.Value.Length)

    [<Fact>]
    let ``parse string type with custom extensions and restrictions`` () =
        let input = """type string {
                 junos:posix-pattern "^.{1,64}$";
                 junos:pattern-message "Must be string of 64 characters or less";
                 length "1 .. 128";
               }"""
        let id, restriction, unknowns = FParsecHelper.apply parse_type_statement input
        Assert.Equal(Identifier.IdentifierReference.Make "string", id)
        Assert.NotEqual(None, restriction)
        Assert.NotEqual(None, unknowns)
        Assert.Equal(2, unknowns.Value.Length)

    [<Fact>]
    let ``fail when string type length restriction appears twice`` () =
        let input = """type string {
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
             length "1 .. 64";
             length "1 .. 128";
           }"""

        Assert.ThrowsAny<Exception>(
            fun _ -> FParsecHelper.apply parse_type_statement input |> ignore
        )

    [<Fact>]
    let ``parse string type with both length and pattern restriction`` () =
        let input = """type string {
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
             length "1 .. 64";
             pattern "^.{1,64}$";
           }"""

        let id, restriction, unknowns = FParsecHelper.apply parse_type_statement input
        Assert.Equal(Identifier.IdentifierReference.Make "string", id)
        Assert.NotEqual(None, restriction)
        Assert.NotEqual(None, unknowns)
        Assert.Equal(2, unknowns.Value.Length)

        match restriction.Value with
        | TypeBodyStatement.StringRestrictions ((Some length), pattern) ->
            Assert.Equal(1, pattern.Length)
        | TypeBodyStatement.StringRestrictions (restriction, pattern) ->
            Assert.True(false, sprintf "Expected to find a string length restriction, got None in %A" restriction.Value)
        | _ ->
            Assert.True(false, sprintf "Expected to find a string restriction statement; found %A" restriction.Value)

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

        let id, restriction, unknowns = FParsecHelper.apply parse_type_statement input
        Assert.Equal(Identifier.IdentifierReference.Make "string", id)
        Assert.NotEqual(None, restriction)
        Assert.NotEqual(None, unknowns)
        Assert.Equal(2, unknowns.Value.Length)

        match restriction.Value with
        | TypeBodyStatement.StringRestrictions (length, pattern) ->
            Assert.NotEqual(None, length)
            Assert.Equal(3, pattern.Length)
        | _ ->
            Assert.True(false, sprintf "Expected to find a string restriction statement; found %A" restriction.Value)