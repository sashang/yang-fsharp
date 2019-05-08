namespace Yang.Parser.Tests

module UtilitiesTests =
    open System
    open Xunit
    open FParsec
    open Yang.Parser

    [<Fact>]
    let ``fail when pip does not consume entire input`` () =
        let input = """ "inner label" """
        Assert.ThrowsAny<Exception>(
            fun _ ->
                FParsecHelper.apply (
                    spaces >>. (pip Strings.parse_string Identifier.parse_identifier)
                ) input
                |> ignore
        )

    [<Fact>]
    let ``pip_pstring fail test when receiving unexpected string`` () =
        let input = """ "inner label" """
        Assert.ThrowsAny<Exception>(
            fun _ ->
                FParsecHelper.apply (
                    (spaces >>. (pip_pstring Strings.parse_string "errorneous"))
                ) input
                |> ignore
        )
