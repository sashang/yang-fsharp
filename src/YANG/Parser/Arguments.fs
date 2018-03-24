// Tokens.fs
// Parsing of basic tokens of the YANG model

namespace Yang.Parser

module Arguments =
    open System
    open FParsec
    open Yang.Model.Arguments

    // Below we define a custom date field. We could have used the system DateTime,
    // but that gives more information (time) that specified by the grammar.

    let parse_boolean<'a> : Parser<bool, 'a> =
            (skipString "true"  .>> spaces |>> (fun _ -> true))
        <|> (skipString "false" .>> spaces |>> (fun _ -> false))
        <|> (skipString "'true'"  .>> spaces |>> (fun _ -> true))
        <|> (skipString "'false'" .>> spaces |>> (fun _ -> false))
        <|> (skipString "\"true\""  .>> spaces |>> (fun _ -> true))
        <|> (skipString "\"false\"" .>> spaces |>> (fun _ -> false))

    /// Parses a YANG date
    let parse_date<'a> : Parser<Date, 'a> =
        // From [RFC 7950, page 206]
        // date-arg-str        = < a string that matches the rule >
        //                       < date-arg >
        // date-arg            = 4DIGIT "-" 2DIGIT "-" 2DIGIT

        regex "\d{4}\-\d{2}-\d{2}" .>> spaces
        |>> (fun date_string ->
            let value index = int (date_string.Chars index) - int ('0')
            let year = (value 0) * 1000 + (value 1) * 100 + (value 2) * 10 + value 3
            let month = (value 5) * 10 + value 6
            let day = (value 8) * 10 + value 9
            Date.Make (year, month, day)
        )

    let parse_fraction_digits<'a> : Parser<byte, 'a> =
        // [RFC 7950, 189]
        //fraction-digits-arg-str = < a string that matches the rule >
        //                            < fraction-digits-arg >
        //fraction-digits-arg = ("1" ["0" / "1" / "2" / "3" / "4" /
        //                            "5" / "6" / "7" / "8"])
        //                        / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"
        // TODO: Proper parsing of fraction-digits; it could be a string in general
        puint8 .>> spaces

    let parse_length_boundary<'a> : Parser<LengthBoundary, 'a> =
            (skipString "min"   |>> fun _ -> LengthBoundary.Min)
        <|> (skipString "max"   |>> fun _ -> LengthBoundary.Max)
        <|> (puint64            |>> LengthBoundary.Number)
        .>> spaces

    let parse_length_part<'a> : Parser<LengthPart, 'a> =
        parse_length_boundary .>> spaces .>>. (opt (skipString ".." >>. spaces >>. parse_length_boundary)) .>> spaces
        |>> (
            fun (left, right) ->
                match right with
                | None          -> LengthPart.Single left
                | Some right    -> LengthPart.Range (left, right)
        )
        .>> spaces

    let parse_length<'a> : Parser<Length, 'a> =
        // [RFC 7950, page 204]
        //length-arg-str      = < a string that matches the rule >
        //                        < length-arg >
        //length-arg          = length-part *(optsep "|" optsep length-part)
        //length-part         = length-boundary
        //                        [optsep ".." optsep length-boundary]
        //length-boundary     = min-keyword / max-keyword /
        //                        non-negative-integer-value
        // TODO: Proper parsing of length
        (sepBy1 parse_length_part (spaces >>. skipChar '|' .>> spaces)) |>> (fun s -> Length s)
        .>> spaces

    let parse_max_value<'a> : Parser<MaxValue, 'a> =
        // [RFC 7950, p.192, 207 and 209]
        //max-value-arg-str   = < a string that matches the rule >
        //                        < max-value-arg >
        //max-value-arg       = unbounded-keyword /
        //                        positive-integer-value
        //unbounded-keyword        = %s"unbounded"
        //positive-integer-value = (non-zero-digit *DIGIT)
            (skipString "unbounded" |>> (fun _ -> MaxValue.Unbounded))
        <|> (puint64                |>> (fun value -> MaxValue.Bounded value)) .>> spaces

    let parse_min_value<'a> : Parser<MinValue, 'a> =
        // [RFC 7950, p.192]
        //min-value-arg-str   = < a string that matches the rule >
        //                        < min-value-arg >
        //min-value-arg       = non-negative-integer-value
            puint32 |>> MinValue

    let parse_modifier<'a> : Parser<Modifier, 'a> =
        // [RFC 7950, p. 190]
        //modifier-arg-str    = < a string that matches the rule >
        //                        < modifier-arg >
        //modifier-arg        = invert-match-keyword
        (skipString "invert-match" |>> (fun _-> Modifier.InvertMatch)) .>>
        spaces

    let parse_ordered_by<'a> : Parser<OrderedBy, 'a> =
        // [RFC 7950, p. 192]
        //ordered-by-arg-str  = < a string that matches the rule >
        //                        < ordered-by-arg >
        //ordered-by-arg      = user-keyword / system-keyword
                (skipString "user"      |>> (fun _ -> OrderedBy.User))
            <|> (skipString "system"    |>> (fun _ -> OrderedBy.System))
            .>> spaces

    let parse_range_boundary<'a> : Parser<RangeBoundary, 'a> =
        let numberFormat =     NumberLiteralOptions.AllowMinusSign
                           ||| NumberLiteralOptions.AllowFraction

        (skipString "min"   |>> fun _ -> RangeBoundary.Min)
        <|> (skipString "max"   |>> fun _ -> RangeBoundary.Max)
        <|> (numberLiteral numberFormat "range boundary"
             |>> fun v ->
                    if v.HasFraction then RangeBoundary.Decimal (System.Decimal.Parse(v.String))
                    else RangeBoundary.Integer (System.Numerics.BigInteger.Parse v.String)
            )
        .>> spaces

    let parse_range_part<'a> : Parser<RangePart, 'a> =
        parse_range_boundary .>> spaces .>>. (opt (skipString ".." >>. spaces >>. parse_range_boundary)) .>> spaces
        |>> (
            fun (left, right) ->
                match right with
                | None          -> RangePart.Single left
                | Some right    -> RangePart.Region (left, right)
        )
        .>> spaces

    let parse_range<'a> : Parser<Range, 'a> =
        // [RFC 7950, p. 204]
        //range-arg-str       = < a string that matches the rule >
        //                        < range-arg >
        //range-arg           = range-part *(optsep "|" optsep range-part)
        //range-part          = range-boundary
        //                        [optsep ".." optsep range-boundary]
        //range-boundary      = min-keyword / max-keyword /
        //                        integer-value / decimal-value
        sepBy1 parse_range_part (spaces >>. skipChar '|' .>> spaces)
        |>> Range
        .>> spaces

    let parse_status<'a> : Parser<Status, 'a> =
            (skipString "current"       >>. spaces  |>> (fun _ -> Status.Current))
        <|> (skipString "obsolete"      >>. spaces  |>> (fun _ -> Status.Obsolete))
        <|> (skipString "deprecated"    >>. spaces  |>> (fun _ -> Status.Deprecated))
