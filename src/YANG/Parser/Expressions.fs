// Expressions.fs
namespace Yang.Parser

module Expressions =
    open FParsec
    open Yang.Model
    open Yang.Model.Expressions

    // [RFC7950, p.188]
    //if-feature-expr     = if-feature-term
    //                        [sep or-keyword sep if-feature-expr]
    //
    //if-feature-term     = if-feature-factor
    //                        [sep and-keyword sep if-feature-term]
    //
    //if-feature-factor   = not-keyword sep if-feature-factor /
    //                        "(" optsep if-feature-expr optsep ")" /
    //                        identifier-ref-arg

    let parse_expression<'a> : Parser<Expression, 'a> =
        let (parse_expression : Parser<Expression, 'a>),
            (parse_expression_ref : Parser<Expression, 'a> ref)
         = createParserForwardedToRef<Expression, 'a>()

        let (parse_factor : Parser<Factor, 'a>),
            (parse_factor_ref : Parser<Factor, 'a> ref)
         = createParserForwardedToRef<Factor, 'a>()

        let parse_factor_implementation : Parser<Factor, 'a> =
                (skipString "not" >>. spaces >>. parse_factor .>> spaces
                 |>> Factor.Not)
            <|> (skipChar '(' >>. spaces >>. parse_expression .>>
                 spaces .>> skipChar ')' .>> spaces
                 |>> Factor.Expression)
            <|> (Identifier.parse_identifier_reference .>> spaces
                 |>> Factor.Identifier)

        let parse_term : Parser<Term, 'a> =
            sepBy1 parse_factor_implementation
                   (skipString "and" >>. spaces)

        let parse_expression_implementation : Parser<Expression, 'a> =
            sepBy1 parse_term (skipString "or" >>. spaces)

        parse_factor_ref := parse_factor_implementation
        parse_expression_ref := parse_expression_implementation

        parse_expression

    /// Parses an if-feature-statement
    let parse_if_feature_statement<'a> : Parser<IfFeatureStatement, 'a> =
        make_statement_parser_optional
            "if-feature"
            (Utilities.pip Strings.parse_string parse_expression)
            parse_statement
