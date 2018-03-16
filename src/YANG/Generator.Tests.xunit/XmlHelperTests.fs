namespace Yang.Generator.Tests

module XmlHelperTests =
    open Xunit
    open Yang.Generator
    open Yang.Generator.XmlHelper

    [<Fact>]
    let ``parsing empty string should return none`` () =
        let result = (XmlReaderHelper.Parse """<name></name>""").ReadString "name"
        Assert.Equal(None, result)

    [<Fact>]
    let ``parsing simple string`` () =
        let result = (XmlReaderHelper.Parse """<name>1.2.0.1/31</name>""").ReadString "name"
        Assert.Equal(Some "1.2.0.1/31", result)

    [<Fact>]
    let ``parsing wrong node should fail`` () =
        Assert.ThrowsAny<XmlParsingException>(
            fun _ -> (XmlReaderHelper.Parse """<name>1.2.0.1/31</name>""").ReadString "address" |> ignore
        )

    [<Fact>]
    let ``parser should ignore comments at the beginning of line`` () =
        let result = (XmlReaderHelper.Parse """<name><!-- ignore this-->1.2.0.1/31</name>""").ReadString "name"
        Assert.Equal(Some "1.2.0.1/31", result)

    [<Fact>]
    let ``parser should ignore comments at the end of line`` () =
        let result = (XmlReaderHelper.Parse"""<name>1.2.0.1/31<!-- ignore this--></name>""").ReadString "name"
        Assert.Equal(Some "1.2.0.1/31", result)

