// Tokens.fs
// Parsing of basic tokens of the YANG model

namespace Yang.Parser

module Arguments =
    open System
    open System.Numerics
    open System.Text
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

        (skipString "min"   |>> fun _ -> RangeBoundary.Min)
        <|> (skipString "max"   |>> fun _ -> RangeBoundary.Max)
        <|> (numberLiteral numberFormat "range boundary"
             |>> fun v ->
                    if v.HasFraction then RangeBoundary.Decimal (System.Decimal.Parse(v.String))
                    else RangeBoundary.Integer (System.Numerics.BigInteger.Parse v.String)
            )
        .>> spaces

    [<Struct>]
    type private RangePartState =
    | First
    | RangeSearch
    | Second
    | RangeError of string

    [<Struct>]
    type private BoundaryType =
    | BoundaryMin
    | BoundaryMax
    | BoundaryInteger
    | BoundaryDecimal

    let private parse_range_part_implementation<'a> (input : CharStream<'a>) : Reply<Range.RangePart> =
        let left = StringBuilder ()
        let right = StringBuilder ()

        let left_type  : BoundaryType option ref = ref None
        let right_type : BoundaryType option ref = ref None

        let skip ()  = input.Read() |> ignore
        let skip2 () = input.Read() |> ignore; input.Read() |> ignore

        let rec advance (state : RangePartState) =
            if input.IsEndOfStream then state
            else scan state
        and apply_minus (sb : StringBuilder, ty : BoundaryType option ref, state) =
            if (!ty).IsNone then
                ty  := Some BoundaryInteger
                sb.Append(input.Read()) |> ignore
                advance state
            else
                RangeError "Unexpected character '-'"
        and apply_keyword (ty : BoundaryType option ref, state, should_continue) =
            if (!ty).IsSome then
                RangeError "Unexpected character 'm' in number"
            else
                skip()
                let next = input.Peek2()
                match next.Char0, next.Char1 with
                | 'i', 'n'  ->
                    skip2 ()
                    let after = input.Peek()
                    if isLetter after || isDigit after then
                        RangeError "Expecting 'min' literal"
                    else
                        ty := Some BoundaryMin
                        if should_continue then advance state
                        else state
                | 'a', 'x'  ->
                    skip2 ()
                    let after = input.Peek()
                    if isLetter after || isDigit after then
                        RangeError "Expecting 'max' literal"
                    else
                        ty := Some BoundaryMax
                        if should_continue then advance state
                        else state
                | _, _      ->
                    RangeError "Expecting either min or max"
        and apply_digit (sb : StringBuilder, ty : BoundaryType option ref, state) =
            let d = input.Read()
            if (!ty).IsNone then ty := Some BoundaryInteger
            sb.Append(d) |> ignore
            advance state
        and scan (state : RangePartState) =
            match input.Peek(), state with
            | _  , RangeError _                         -> state
            | '|', _                                    -> state
            | ch , First    when Char.IsWhiteSpace(ch)  -> skip(); advance RangeSearch
            | ch , state    when Char.IsWhiteSpace(ch)  -> skip(); advance state

            | '.', First    ->
                skip()
                let next = input.Peek()
                if next = '.'     then
                    skip()
                    advance Second
                elif isDigit next then
                    left.Append('.') |> ignore
                    // If there are two periods in the number, we can continue parsing here,
                    // and expect to fail later when trying to recover the number from the string
                    left_type := Some BoundaryDecimal
                    advance First
                else
                    RangeError "Expected a digit"

            | '.', Second   ->
                right.Append(input.Read()) |> ignore
                right_type := Some BoundaryDecimal
                advance Second

            | '.', RangeSearch    ->
                skip ()
                let next = input.Read()
                if next = '.' then advance Second
                else RangeError "Cannot detect '..'"

            | '-', First    -> apply_minus (left, left_type, First)
            | '-', Second   -> apply_minus (right, right_type, Second)
            | '-', _        ->
                // Probably we reached here in error. In any case, stop search
                // and expect next parser to fail
                state

            | 'm', First    -> apply_keyword (left_type, RangeSearch, true)
            | 'm', Second   -> apply_keyword (right_type, Second, false)
            | 'm', _        -> RangeError "Unexpected character 'm'"

            | d, First  when isDigit(d) -> apply_digit(left, left_type, First)
            | d, Second when isDigit(d) -> apply_digit(right, right_type, Second)

            | _, _  ->
                if input.IsEndOfStream then state
                else RangeError "Unexpected character"


        let make (sb : StringBuilder, ty : BoundaryType option) =
            match ty with
            | None                  -> Result.Error (ErrorMessage.Expected "Expected to find integer or decimal")
            | Some BoundaryMin      -> Result.Ok    (Range.RangeBoundary.Min)
            | Some BoundaryMax      -> Result.Ok    (Range.RangeBoundary.Max)
            | Some BoundaryDecimal  ->
                match Decimal.TryParse (sb.ToString()) with
                | false, _ -> Result.Error (ErrorMessage.Expected "Expected to find decimal")
                | true, v  -> Result.Ok    (Range.RangeBoundary.Decimal v)
            | Some BoundaryInteger  ->
                match BigInteger.TryParse (sb.ToString()) with
                | false, _ -> Result.Error (ErrorMessage.Expected "Expected to find integer")
                | true, v  -> Result.Ok    (Range.RangeBoundary.Integer v)

        let result = advance First
        match result with
        | First
        | RangeSearch ->
            match make (left, !left_type) with
            | Result.Error error -> Reply(Error, ErrorMessageList error)
            | Result.Ok    value -> Reply(Range.RangePart.Single value)

        | Second ->
            match make (left, !left_type), make (right, !right_type) with
            | Result.Error error, _ ->
                let msg = ErrorMessage.Message "Error parsing the left boundary"
                Reply(Error, ErrorMessageList (msg, error))
            | _, Result.Error error ->
                let msg = ErrorMessage.Message "Error parsing the right boundary"
                Reply(Error, ErrorMessageList (msg, error))
            | Result.Ok left_boundary, Result.Ok right_boundary ->
                if left_boundary = right_boundary then
                    Reply(Range.RangePart.Single left_boundary)
                elif (left_boundary :> IComparable<RangeBoundary>).CompareTo(right_boundary) > 0 then
                    let msg = ErrorMessage.Message "Left boundary cannot be larger than right boundary"
                    Reply(Error, ErrorMessageList(msg))
                else
                    Reply(Range.RangePart.Region (left_boundary, right_boundary))

        | RangeError message ->
            Reply(Error, ErrorMessageList (ErrorMessage.Message message))

    let parse_range_part<'a> : Parser<RangePart, 'a> =
        parse_range_part_implementation
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
