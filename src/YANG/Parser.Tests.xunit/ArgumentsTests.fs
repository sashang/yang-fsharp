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
        let result = FParsecHelper.apply parse_range input
        Assert.Equal(Range [ (RangePart.Single (RangeBoundary.Integer 15L)) ], result)

    [<Fact>]
    let ``parse range of negative integer number value`` () =
        let input = "-15"
        let result = FParsecHelper.apply parse_range input
        Assert.Equal(Range [ (RangePart.Single (RangeBoundary.Integer -15L)) ], result)

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
        Assert.True(System.Math.Abs(value.Value + 3.14M) < 0.01M)

    [<Fact>]
    let ``parse range of set of values`` () =
        let input = "'15 .. 20'"
        let result = FParsecHelper.apply parse_range input
        Assert.Equal(Range [ (RangePart.Region (RangeBoundary.Integer 15L, RangeBoundary.Integer 20L)) ], result)

    // TODO: Add more tests for range-arg parser
