namespace Yang.Parser.Tests

module ModuleBodyTests =
    open System
    open Xunit
    open FParsec
    open Yang.Model
    open Yang.Parser

    [<Fact>]
    let ``parse empty module body`` () =
        let (header, meta, revisions) =
            FParsecHelper.apply
                (tuple3 Header.parse_header Meta.parse_meta Revisions.parse_revision_list)
                ""

        Assert.Null(header)
        Assert.Null(meta)
        Assert.NotNull(revisions)
        Assert.Empty(revisions)

    [<Fact>]
    let ``parse module header statements`` () =
        let body = """yang-version 1.1;
namespace "urn:example:system";
prefix "sys";

organization "Example Inc.";
contact "joe@example.com";
description
    "The module for entities implementing the Example system.";

revision 2007-06-09 {
    description "Initial revision.";
}
"""
        let (header, meta, revisions) =
            FParsecHelper.apply
                (tuple3 Header.parse_header Meta.parse_meta Revisions.parse_revision_list)
                body

        Assert.NotNull(header)
        let version, ns, prefix, unknown_header = header

        Assert.NotNull(meta)
        Assert.NotNull(revisions)

        Assert.Equal((Version (1, 1), None),            version)
        Assert.Equal((Uri("urn:example:system"), None), ns)
        Assert.Equal(("sys", None),                     prefix)
        Assert.Equal(None,                              unknown_header)

        Assert.Equal(Some ("Example Inc.", None),       MetaStatements.Organization meta)
        Assert.Equal(Some ("joe@example.com", None),    MetaStatements.Contact meta)
        Assert.Equal(Some ("The module for entities implementing the Example system.", None),
                     MetaStatements.Description meta)
        Assert.Equal(None, MetaStatements.Reference meta)
        Assert.Equal(None, MetaStatements.Unknown meta)

        Assert.NotEmpty(revisions)
        Assert.Equal(1, revisions.Length)
        match revisions with
        | (date, _) as rev :: [] ->
            Assert.Equal(2007us,    date.Year)
            Assert.Equal(06uy,      date.Month)
            Assert.Equal(09uy,      date.Day)
            Assert.Equal(Some ("Initial revision.", None), RevisionStatement.Description rev)
            Assert.Equal(None, RevisionStatement.Unknown rev)
            Assert.Equal(None, RevisionStatement.Reference rev)
        | _ -> failwith "Internal error: unit test should not have reached this point"

    // TODO: more extensive unit tests for module body
