namespace Yang.Parser.Tests

module PathArgumentTests =
    open Xunit
    open Yang.Model.Arguments
    open Yang.Parser.PathArgument
    open Yang.Model.Identifier

    [<Fact>]
    let ``parse simple absolute path`` () =
        let input ="/fiber:fiber-wavelength-profiles/fiber:wavelength-profile/fiber:name"
        let path = FParsecHelper.apply parse_path input
        Assert.True(path._IsAbsolute)
        let p = path.AsAbsolute
        Assert.True(p.IsSome)
        Assert.True(p.Value.IsValid)
        let items = p.Value.Path
        Assert.Equal(3, items.Length)
        let item1 = List.item 0 items
        let item2 = List.item 1 items
        let item3 = List.item 2 items

        Assert.Equal("fiber:fiber-wavelength-profiles", item1.Value)
        Assert.Equal("fiber:wavelength-profile", item2.Value)
        Assert.Equal("fiber:name", item3.Value)
        Assert.False(item1.HasPredicate)
        Assert.False(item2.HasPredicate)
        Assert.False(item3.HasPredicate)

    [<Fact>]
    let ``parse simple relative path`` () =
        let input = "../interface/name"
        let path = FParsecHelper.apply parse_path input
        Assert.True(path._IsRelative)
        let p = path.AsRelative
        Assert.True(p.IsSome)
        Assert.True(p.Value.IsValid)
        Assert.Equal(1us, p.Value.UpSteps)
        let items = p.Value.Path
        Assert.Equal(2, items.Length)
        let item1 = List.item 0 items
        let item2 = List.item 1 items
        Assert.Equal("interface", item1.Value)
        Assert.Equal("name", item2.Value)
        Assert.False(item1.HasPredicate)
        Assert.False(item2.HasPredicate)

    [<Fact>]
    let ``parse simple relative path with 2 levels up`` () =
        let input = "../../interface/name"
        let path = FParsecHelper.apply parse_path input
        Assert.True(path._IsRelative)
        let p = path.AsRelative
        Assert.True(p.IsSome)
        Assert.True(p.Value.IsValid)
        Assert.Equal(2us, p.Value.UpSteps)
        let items = p.Value.Path
        Assert.Equal(2, items.Length)
        let item1 = List.item 0 items
        let item2 = List.item 1 items
        Assert.Equal("interface", item1.Value)
        Assert.Equal("name", item2.Value)
        Assert.False(item1.HasPredicate)
        Assert.False(item2.HasPredicate)

    [<Fact>]
    let ``parse relative path with predicate`` () =
        let input = "../../interface[name = current()/../ifname]/address/ip"
        let path = FParsecHelper.apply parse_path input
        Assert.True(path._IsRelative)
        let p = path.AsRelative
        Assert.True(p.IsSome)
        Assert.True(p.Value.IsValid)
        Assert.Equal(2us, p.Value.UpSteps)

        let items = p.Value.Path
        Assert.Equal(3, items.Length)
        let item1 = List.item 0 items
        let item2 = List.item 1 items
        let item3 = List.item 2 items

        Assert.Equal("address", item2.Value)
        Assert.Equal("ip", item3.Value)
        Assert.False(item2.HasPredicate)
        Assert.False(item3.HasPredicate)

        Assert.Equal("interface[name = current()/../ifname]", item1.Value)
        Assert.True(item1.HasPredicate)
        let predicate = item1._Predicate
        Assert.True(predicate.IsSome)
        Assert.Equal(1, predicate.Value.Length)
        let pred = predicate.Value.Head
        Assert.Equal(IdentifierReference.Make "name", pred._Node)

        let key = pred._Key
        Assert.True(key.IsValid)
        Assert.Equal(1us, key._Up)
        let nodes = key._Node
        Assert.Equal(1, nodes.Length)
        let n = nodes.Head
        Assert.True(n.IsValid)
        Assert.Equal("ifname", n.Value)


    [<Theory>]
    [<InlineData("/fiber:fiber-wavelength-profiles/fiber:wavelength-profile/fiber:name")>]
    [<InlineData("../interface/name")>]
    [<InlineData("../../interface/name")>]
    [<InlineData("../../interface[name = current()/../ifname]/address/ip")>]
    //[<InlineData("")>]
    let ``reconstruct path string`` (input) =
        let path = FParsecHelper.apply parse_path input
        Assert.Equal(input, path.Value)

