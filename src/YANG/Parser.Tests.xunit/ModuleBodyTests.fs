namespace Yang.Parser.Tests

module ModuleBodyTests =
    open System
    open Xunit
    open FParsec
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
        Assert.NotNull(meta)
        Assert.NotNull(revisions)

        Assert.Equal((Version (1, 1), None),            header.YangVersion)
        Assert.Equal((Uri("urn:example:system"), None), header.Namespace)
        Assert.Equal(("sys", None),                     header.Prefix)
        Assert.Equal(None,                              header.Options)

        Assert.Equal(Some ("Example Inc.", None),     meta.Organization)
        Assert.Equal(Some ("joe@example.com", None),  meta.Contact)
        Assert.Equal(Some ("The module for entities implementing the Example system.", None),
                     meta.Description)
        Assert.Equal(None, meta.Reference)
        Assert.Equal(None, meta.Options)

        Assert.NotEmpty(revisions)
        Assert.Equal(1, revisions.Length)
        match revisions with
        | rev :: [] ->
            Assert.Equal(2007us,    rev.Version.Year)
            Assert.Equal(06uy,      rev.Version.Month)
            Assert.Equal(09uy,      rev.Version.Day)
            Assert.Equal(Some ("Initial revision.", None), rev.Description)
            Assert.Equal(None, rev.Options)
            Assert.Equal(None, rev.Reference)
        | _ -> failwith "Internal error: unit test should not have reached this point"
