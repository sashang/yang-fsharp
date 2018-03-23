namespace Yang.Parser.Tests

module ExpressionTests =
    open FParsec
    open Xunit
    open Yang.Model.Identifier
    open Yang.Model.Expressions
    open Yang.Parser.Expressions
    open Yang.Model.Statements

    [<Fact>]
    let ``parse simple if-feature statement with a single term (identifier)`` () =
        // [RFC 7950, page 41]
        let input = "if-feature foo;"
        let (IfFeatureStatement (result, _)) = FParsecHelper.apply parse_if_feature_statement input
        Assert.Equal(1, result.Length)
        Assert.Equal(1, result.Head.Length)
        let id = FactorAsIdentifier result.Head.Head
        Assert.True(id.IsSome)
        Assert.Equal(IdentifierReference.Make "foo", id.Value)

    [<Fact>]
    let ``parse simple if-feature statement with a single term (identifier) 2`` () =
        // [RFC 7950, page 129]
        let input = "if-feature local-storage;"
        let (IfFeatureStatement (result, _)) = FParsecHelper.apply parse_if_feature_statement input
        Assert.Equal(1, result.Length)
        Assert.Equal(1, result.Head.Length)
        let id = FactorAsIdentifier result.Head.Head
        Assert.True(id.IsSome)
        Assert.Equal(IdentifierReference.Make "local-storage", id.Value)

    [<Fact>]
    let ``parse if-feature statement with or`` () =
        // [RFC 7950, page 131]
        let input = """if-feature "outbound-tls or outbound-ssh";"""
        let (IfFeatureStatement (result, _)) = FParsecHelper.apply parse_if_feature_statement input
        Assert.Equal(2, result.Length)
        Assert.Equal(1, result.Head.Length)
        Assert.Equal(1, result.Tail.Head.Length)

        let id1 = FactorAsIdentifier result.Head.Head
        Assert.True(id1.IsSome)
        Assert.Equal(IdentifierReference.Make "outbound-tls", id1.Value)

        let id2 = FactorAsIdentifier result.Tail.Head.Head
        Assert.True(id2.IsSome)
        Assert.Equal(IdentifierReference.Make "outbound-ssh", id2.Value)

    [<Fact>]
    let ``parse if-feature statement with all operators`` () =
        // [RFC 7950, page 131]
        let input = """if-feature "not foo or bar and baz";"""
        let (IfFeatureStatement (result, _)) = FParsecHelper.apply parse_if_feature_statement input
        Assert.Equal(2, result.Length)
        Assert.Equal(1, result.Head.Length)
        Assert.Equal(2, result.Tail.Head.Length)

        Assert.True(IsFactorNegation result.Head.Head)

    [<Fact>]
    let ``parse if-feature statement with all operators, with parenthesis`` () =
        // [RFC 7950, page 131]
        let input = """if-feature "(not foo) or (bar and baz)";"""
        let (IfFeatureStatement (result, _)) = FParsecHelper.apply parse_if_feature_statement input
        Assert.Equal(2, result.Length)
        Assert.Equal(1, result.Head.Length)
        Assert.Equal(1, result.Tail.Length)
        Assert.Equal(1, result.Tail.Head.Length)

        let expr = FactorAsExpression result.Tail.Head.Head
        Assert.True(expr.IsSome)
        Assert.Equal(1, expr.Value.Length)
        Assert.Equal(2, expr.Value.Head.Length)

        let expr2 = FactorAsExpression result.Head.Head
        Assert.True(expr2.IsSome)
        Assert.Equal(1, expr2.Value.Length)
        Assert.Equal(1, expr2.Value.Head.Length)
        Assert.True(IsFactorNegation expr2.Value.Head.Head)

    [<Fact>]
    let ``parse if-feature statement with and`` () =
        // [RFC 7950, page 131]
        let input = """if-feature 'bar and baz';"""
        let (IfFeatureStatement (result, _)) = FParsecHelper.apply parse_if_feature_statement input
        Assert.Equal(1, result.Length)
        Assert.Equal(2, result.Head.Length)

        let id1 = FactorAsIdentifier result.Head.Head
        Assert.True(id1.IsSome)
        Assert.Equal(IdentifierReference.Make "bar", id1.Value)

        let id2 = FactorAsIdentifier result.Head.Tail.Head
        Assert.True(id2.IsSome)
        Assert.Equal(IdentifierReference.Make "baz", id2.Value)

