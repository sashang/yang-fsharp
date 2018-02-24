// Tokens.fs
// Parsing of basic tokens of the YANG model

namespace Yang.Parser

module Tokens =
    open System
    open FParsec

    // Below we define a custome date field. We could have used the system DateTime,
    // but that gives more information (time) that specified by the grammar.

    /// Represents a date (date-arg in YANG).
    [<StructuredFormatDisplay("{Value}")>]
    [<CustomEquality; CustomComparison>]
    type Date = {
        Year    : uint16
        Month   : uint8
        Day     : uint8
    } with
        static member Make(dt : DateTime) = {
            Year    = uint16 dt.Year
            Month   = uint8 dt.Month
            Day     = uint8 dt.Day
        }

        static member Make(year : uint16, month : uint8, day : uint8) =
            // Check that the date is valid; the following throws an exception if not valid
            let _ = DateTime(int year, int month, int day)
            {
                Year    = year
                Month   = month
                Day     = day
            }

        static member Make(year : int, month : int, day : int) = Date.Make (uint16 year, uint8 month, uint8 day)

        member this.Value = sprintf "%04d-%02d-%02d" this.Year this.Month this.Day
        override this.ToString() = this.Value

        member this.AsDateTime = DateTime(int this.Year, int this.Month, int this.Day)

        interface IEquatable<Date> with
            member this.Equals other =
                this.Year   = other.Year &&
                this.Month  = other.Month &&
                this.Day    = other.Day

        override this.Equals other =
            match other with
            | :? Date as other' -> (this :> IEquatable<Date>).Equals(other)
            | _                 -> invalidArg "other" "cannot compare values of different types"

        override this.GetHashCode() =
            this.Year.GetHashCode() ^^^ this.Month.GetHashCode() ^^^ this.Day.GetHashCode()

        interface IComparable<Date> with
            member this.CompareTo other =
                if this.Year <> other.Year then
                    this.Year.CompareTo(other.Year)
                elif this.Month <> other.Month then
                    this.Month.CompareTo(other.Month)
                else
                    this.Day.CompareTo(other.Day)

        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? Date as other' -> (this :> IComparable<Date>).CompareTo(other')
                | _                 -> invalidArg "other" "cannot compare values of different types"

    /// Parses a YANG date
    let parse_date<'a> : Parser<Date, 'a> =
        // From [RFC 7950, page 206]
        // date-arg-str        = < a string that matches the rule >
        //                       < date-arg >
        // date-arg            = 4DIGIT "-" 2DIGIT "-" 2DIGIT

        regex "\d{4}\-\d{2}-\d{2}"
        |>> (fun date_string ->
            let value index = int (date_string.Chars index) - int ('0')
            let year = (value 0) * 1000 + (value 1) * 100 + (value 2) * 10 + value 3
            let month = (value 5) * 10 + value 6
            let day = (value 8) * 10 + value 9
            Date.Make (year, month, day)
        )
