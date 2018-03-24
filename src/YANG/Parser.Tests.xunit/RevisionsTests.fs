namespace Yang.Parser.Tests

open Xunit

[<Collection("Yang Parser")>]
module RevisionsTests =
    open Xunit
    open Yang.Model
    open Yang.Parser
    open Yang.Parser.Revisions

    [<Fact>]
    let ``revision info with no description`` () =
        let revision = """revision 2007-06-09;"""

        let revision = FParsecHelper.apply parse_revision_statement revision
        let (RevisionStatement (date, _)) = revision

        Assert.Equal(2007us,    date.Year)
        Assert.Equal(6uy,       date.Month)
        Assert.Equal(9uy,       date.Day)
        Assert.Equal(None,      RevisionStatement.Description   revision)
        Assert.Equal(None,      RevisionStatement.Reference     revision)
        Assert.Equal(None,      RevisionStatement.Unknown       revision)

    [<Fact>]
    let ``simple revision info with description`` () =
        let revision = """revision 2007-06-09 {
            description "Initial revision.";
        }
"""

        let revision = FParsecHelper.apply parse_revision_statement revision
        let (RevisionStatement (date, _)) = revision

        Assert.Equal(2007us,    date.Year)
        Assert.Equal(6uy,       date.Month)
        Assert.Equal(9uy,       date.Day)
        Assert.Equal(None,      RevisionStatement.Unknown   revision)
        Assert.Equal(None,      RevisionStatement.Reference revision)
        Assert.Equal(Some (DescriptionStatement ("Initial revision.", None)), RevisionStatement.Description revision)

    [<Fact>]
    let ``revision info with description with options`` () =
        let revision = """revision 2007-06-09 {
    description "Initial revision." {
        ex:documentation-flag 5;
    }
}
"""

        let revision = FParsecHelper.apply parse_revision_statement revision
        let (RevisionStatement (date, _)) = revision

        Assert.Equal(2007us,    date.Year)
        Assert.Equal(6uy,       date.Month)
        Assert.Equal(9uy,       date.Day)
        Assert.Equal(None,      RevisionStatement.Unknown   revision)
        Assert.Equal(None,      RevisionStatement.Reference revision)

        Assert.Equal(
            (   DescriptionStatement(
                    "Initial revision.",
                    Some [
                        Statement.Unknown (
                            UnknownStatement (
                                IdentifierWithPrefix.Make "ex:documentation-flag", 
                                Some "5",
                                None
                            )
                        )
                    ]
                )
            ) |> Option.Some,
            RevisionStatement.Description revision
        )

    [<Fact>]
    let ``revision in quotes`` () =
        let revision = """revision "2015-09-11";"""
        let (RevisionStatement (date, extra)) = FParsecHelper.apply parse_revision_statement revision
        Assert.Equal(2015us,    date.Year)
        Assert.Equal(9uy,       date.Month)
        Assert.Equal(11uy,      date.Day)
        Assert.True(extra.IsNone)

    [<Fact>]
    let ``revision in quotes with extra statement`` () =
        let revision = """revision "2015-09-11" {
                            description
                                "IOS XR 5.3.1 revision.";
                          }"""
        let (RevisionStatement (date, extra)) = FParsecHelper.apply parse_revision_statement revision
        Assert.Equal(2015us,    date.Year)
        Assert.Equal(9uy,       date.Month)
        Assert.Equal(11uy,      date.Day)
        Assert.True(extra.IsSome)
        let extras = extra.Value
        Assert.Equal(1, extras.Length)
        let description = extras.Head
        Assert.True(RevisionBodyStatement.IsDescription description)


    // TODO: Add unit test for revision with non-empty reference
    // TODO: Add unit test for revision with unknown statements
    // TODO: Add unit test for revision with description and extra body
    // TODO: Add unit test for revision with non-empty reference and extra body
