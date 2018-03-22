namespace Yang.Parser.Tests

module ArgumentsTests =
    open Xunit
    open Yang.Model.Arguments
    open Yang.Parser.Arguments

    [<Fact>]
    let ``parse length of min value`` () =
        let input = "min"
        let result = FParsecHelper.apply parse_length input
        Assert.Equal(Length [ (LengthPart.Single LengthBoundary.Min) ], result)

    [<Fact>]
    let ``parse length of max value`` () =
        let input = "max"
        let result = FParsecHelper.apply parse_length input
        Assert.Equal(Length [ (LengthPart.Single LengthBoundary.Max) ], result)

    [<Fact>]
    let ``parse length of number value`` () =
        let input = "15"
        let result = FParsecHelper.apply parse_length input
        Assert.Equal(Length [ (LengthPart.Single (LengthBoundary.Number 15uL)) ], result)

    [<Fact>]
    let ``parse length of range values`` () =
        let input = "'15 .. 20'"
        let result = FParsecHelper.apply parse_length input
        Assert.Equal(Length [ (LengthPart.Range (LengthBoundary.Number 15uL, LengthBoundary.Number 20uL)) ], result)

    // TODO: Add more tests for length-arg parser


    [<Fact>]
    let ``parse range of min value`` () =
        let input = "min"
        let result = FParsecHelper.apply parse_range input
        Assert.Equal(Range [ (RangePart.Single RangeBoundary.Min) ], result)

    [<Fact>]
    let ``parse range of max value`` () =
        let input = "max"
        let result = FParsecHelper.apply parse_range input
        Assert.Equal(Range [ (RangePart.Single RangeBoundary.Max) ], result)

    [<Fact>]
    let ``parse range of integer number value`` () =
        let input = "15"
        let (Range result) = FParsecHelper.apply parse_range input
        Assert.Equal(1, result.Length)
        let part = result.Head
        Assert.True(part._IsSingle)
        let single = part.AsSingle
        Assert.True(single.IsSome)
        let boundary = single.Value
        Assert.True(boundary._IsInteger)
        let value = boundary.AsInteger
        Assert.True(value.IsSome)
        Assert.Equal(15L, value.Value)

    [<Fact>]
    let ``parse range of negative integer number value`` () =
        let input = "-15"
        let (Range result) = FParsecHelper.apply parse_range input
        Assert.Equal(1, result.Length)
        let part = result.Head
        Assert.True(part._IsSingle)
        let single = part.AsSingle
        Assert.True(single.IsSome)
        let boundary = single.Value
        Assert.True(boundary._IsInteger)
        let value = boundary.AsInteger
        Assert.True(value.IsSome)
        Assert.Equal(-15L, value.Value)
    [<Fact>]
    let ``parse range of decimal number value`` () =
        let input = "3.14"
        let (Range result) = FParsecHelper.apply parse_range input
        Assert.Equal(1, result.Length)
        let part = result.Head
        Assert.True(part._IsSingle)
        let boundary = part.AsSingle
        Assert.True(boundary.IsSome)
        Assert.True(boundary.Value._IsDecimal)
        let value = boundary.Value.AsDecimal
        Assert.True(value.IsSome)
        Assert.True(System.Math.Abs(value.Value - 3.14M) < 0.01M)

    [<Fact>]
    let ``parse range of negative decimal number value`` () =
        let input = "-3.14"
        let (Range result) = FParsecHelper.apply parse_range input
        Assert.Equal(1, result.Length)
        let part = result.Head
        Assert.True(part._IsSingle)
        let boundary = part.AsSingle
        Assert.True(boundary.IsSome)
        Assert.True(boundary.Value._IsDecimal)
        let value = boundary.Value.AsDecimal
        Assert.True(value.IsSome)
        Assert.True(System.Math.Abs(value.Value + 3.14M) < 0.01M)

    [<Fact>]
    let ``parse range of set of values`` () =
        let input = "'15 .. 20'"
        let (Range result) = FParsecHelper.apply parse_range input
        Assert.Equal(1, result.Length)
        let region = result.Head
        Assert.True(region._IsRegion)
        let r = region.AsRegion
        Assert.True(r.IsSome)
        let (left, right) = r.Value
        Assert.True(left._IsInteger)
        Assert.True(right._IsInteger)
        let li = left.AsInteger
        let ri = right.AsInteger
        Assert.True(li.IsSome)
        Assert.True(ri.IsSome)
        Assert.Equal(15L, li.Value)
        Assert.Equal(20L, ri.Value)

    // TODO: Add more tests for range-arg parser

    [<Fact>]
    let ``parse absolute path`` () =
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

    [<Theory>]
    [<InlineData("/fiber:fiber-wavelength-profiles/fiber:wavelength-profile/fiber:name")>]
    let ``reconstruct path string`` (input) =
        let path = FParsecHelper.apply parse_path input
        Assert.Equal(input, path.Value)
