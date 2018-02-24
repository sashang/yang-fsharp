namespace Yang.Parser.Tests

module RevisionsTests =
    open Xunit
    open Yang.Parser
    open Yang.Parser.Revisions

    [<Fact>]
    let ``revision info with no description`` () =
        let revision = """revision 2007-06-09;"""

        let info = FParsecHelper.apply parse_revision revision

        Assert.Equal(2007us, info.Version.Year)
        Assert.Equal(6uy, info.Version.Month)
        Assert.Equal(9uy, info.Version.Day)
        Assert.Equal(None, info.Description)
        Assert.Equal(None, info.Options)
        Assert.Equal(None, info.Reference)

    [<Fact>]
    let ``simple revision info with description`` () =
        let revision = """revision 2007-06-09 {
            description "Initial revision.";
        }
"""

        let info = FParsecHelper.apply parse_revision revision
        Assert.Equal(2007us, info.Version.Year)
        Assert.Equal(6uy, info.Version.Month)
        Assert.Equal(9uy, info.Version.Day)
        Assert.Equal(None, info.Options)
        Assert.Equal(None, info.Reference)

        Assert.True(info.Description.IsSome)
        match info.Description with
        | Some (description, arguments) ->
            Assert.Equal("Initial revision.", description)
            Assert.Equal(None, arguments)
        | None -> failwith "Internal error: unit test should not have reached this point"

    [<Fact>]
    let ``revision info with description with options`` () =
        let revision = """revision 2007-06-09 {
    description "Initial revision." {
        ex:documentation-flag 5;
    }
}
"""

        let info = FParsecHelper.apply parse_revision revision
        Assert.Equal(2007us, info.Version.Year)
        Assert.Equal(6uy, info.Version.Month)
        Assert.Equal(9uy, info.Version.Day)
        Assert.Equal(None, info.Options)
        Assert.Equal(None, info.Reference)

        Assert.True(info.Description.IsSome)
        match info.Description with
        | Some (description, arguments) ->
            Assert.Equal("Initial revision.", description)
            Assert.True(arguments.IsSome)

            match arguments with
            | Some args ->
                Assert.Equal(1, args.Length)
                match args with
                | hd :: [] ->
                    match hd with
                    | Unknown (id, arg, opts) ->
                        Assert.Equal("ex:documentation-flag", id.Value)
                        Assert.Equal(Some "5", arg)
                        Assert.Equal(None, opts)
                    | _ -> Assert.True(false, "Expecting ex:documentation-flag")
                | _ -> failwith "Internal error: unit test should not have reached this point"
            | None -> failwith "Internal error: unit test should not have reached this point"
        | None -> failwith "Internal error: unit test should not have reached this point"
