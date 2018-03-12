// Arguments.fs
// Basic tokens of the YANG model

namespace Yang.Model

module Arguments =
    open System

    // TODO: Fill in the details of augment-arg
    type Augment = | NA
    with
        // TODO: String version of augment-arg
        member this.Value = "NA"

    // Below we define a custom date field. We could have used the system DateTime,
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

    // TODO: Fill details for deviation-arg
    type Deviation = | NA
    with
        // TODO: Fill implementation of deviation argument
        member this.Value = "NA"

    // TODO: Fill details for key-arg
    type Key = IdentifierReference list

    /// Helper methods for the Statement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Key =
        /// Create a key from a string
        let MakeFromString (keys : string) : Key =
            if String.IsNullOrWhiteSpace(keys) then invalidArg "keys" "keys cannot be null or whitespace"
            let keys = keys.Split([| ' '; '\t'; '\r'; '\t' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
            keys |> List.map (IdentifierReference.Make)

        let Value (key : Key) =
            key |> List.map (fun k -> k.Value) |> String.concat " "

    // TODO: Fill detail of length-arg (currently it is just string)
    type Length = | Length of string
    with
        member this.Value = let (Length v) = this in v

    type MaxValue =
    | Unbounded
    // TODO: The MaxValue must be greater than zero (and never zero)
    | Bounded   of uint64
    with
        member this.Value =
            // TODO: Implement convert to string for MaxValue
            sprintf "%A" this

    // TODO: Fill details of modifier-arg
    type Modifier =
    | InvertMatch
    with
        member this.Value = "invert-match"

    // TODO: Fill details for min-value-arg
    type MinValue = | MinValue of uint32
    with
        static member Make (value : uint32) = MinValue value
        member this.Value = let (MinValue mv) = this in sprintf "%d" mv

    // TODO: Fill help methods for OrderedBy
    type OrderedBy =
    | User
    | System
    with
        member this.Value =
            match this with
            | User      -> "user"
            | System    -> "system"

    // TODO: Fill definition for Path
    type Path = | NA
    with
        member this.Value = "NA"

    let PathListValue (path : Path list) = path |> List.map (fun p -> p.Value) |> String.concat " "

    /// Definition of Range ([RFC 7950, p. 204])
    // TODO: Expand definition of Range
    type Range = | Range of string
    with
        static member Make (s : string) =
            Range s

        member this.Value = let (Range r) = this in r

    /// Captures the 'refine-arg' definition ([RFC 7950, p. 198])
    // TODO: Expand definition of Refine
    type Refine = NA
    with
        member this.Value = "NA"

    // TODO: Fill helper methods for Status
    type Status =
    | Current
    | Obsolete
    | Deprecated
    with
        member this.Value =
            match this with
            | Current       -> "current"
            | Obsolete      -> "obsolete"
            | Deprecated    -> "deprecated"

    // TODO: Fill definition of uses-augment-arg
    type UsesAugment = | NA
    with
        member this.Value = "NA"

    // TODO: Fill definition of unique-arg
    type Unique = | NA
    with
        member this.Value = "NA"

    let BoolAsString v = if v then "true" else "false"
