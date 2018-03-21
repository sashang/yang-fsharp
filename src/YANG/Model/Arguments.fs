// Arguments.fs
// Basic tokens of the YANG model

namespace Yang.Model

module Arguments =
    open System
    open NLog

        /// Logger for this module
    let private _logger = LogManager.GetCurrentClassLogger()

    let private throw fmt =
        let do_throw (message : string) =
            _logger.Error message
            raise (YangModelException message)
        Printf.ksprintf do_throw fmt

#if INTERACTIVE
    // The following are used only in interactive (fsi) to help with enabling disabling
    // logging for particular modules.

    type internal Marker = interface end
    let _full_name = typeof<Marker>.DeclaringType.FullName
    let _name = typeof<Marker>.DeclaringType.Name
#endif

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


    [<StructuredFormatDisplay("{Value}")>]
    type Key = | Key of IdentifierReference list
    with
        static member Make (keys : string) =
            if String.IsNullOrWhiteSpace(keys) then invalidArg "keys" "keys cannot be null or whitespace"
            let keys = keys.Split([| ' '; '\t'; '\r'; '\t' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
            Key (keys |> List.map (IdentifierReference.Make))

        member this.Value =
            let (Key keys) = this
            keys |> List.map (fun k -> k.Value) |> String.concat " "

        override this.ToString() = this.Value

    let KeyFromString (keys : string) = Key.Make keys

    [<AutoOpen>]
    module Length =
        [<StructuredFormatDisplay("{Value}")>]
        type LengthBoundary =
        | Min
        | Max
        | Number of uint64
        with
            // TODO: Add comparison support for LengthBoundary

            member this.Value =
                match this with
                | Min       -> "min"
                | Max       -> "max"
                | Number v  -> sprintf "%d" v

            override this.ToString() = this.Value

        [<StructuredFormatDisplay("{Value}")>]
        type LengthPart =
        | Single of LengthBoundary
        | Range of LengthBoundary * LengthBoundary
        with
            static member Make(value : uint64) =
                Single (Number value)

            static member Make(left, right) =
                // TODO: Check that the left boundary is smaller than the right
                Range (Number left, Number right)

            member this.Value =
                match this with
                | Single boundary       -> boundary.Value
                | Range (left, right)   -> sprintf "%s .. %s" left.Value right.Value

            override this.ToString() = this.Value

            member this.IsInRange value =
                match this with
                | Single (Min _)
                | Single (Max _)        -> false
                | Single (Number v)     -> v = value
                | Range (left, right)   ->
                    match left, right with
                    | _, Min
                    | Max, _        -> throw "Invalid length range: %s .. %s" left.Value right.Value
                    | Min, Max      ->
                        _logger.Warn("Detected trivial range that accepts all lengths")
                        true
                    | Min, Number v -> value <= v
                    | Number v, Max -> value >= v
                    | Number left, Number right     -> left <= value && value <= right

        [<StructuredFormatDisplay("{Value}")>]
        type Length = | Length of LengthPart list
        with
            member this.Value = let (Length v) = this in v |> List.map (fun v' -> v'.Value) |> String.concat " | "
            override this.ToString() = this.Value

            member this.IsInRange value =
                let (Length v) = this
                v |> List.exists (fun part -> part.IsInRange value)


    /// Captures the max-value-arg from [RFC 7950, p. 192]
    [<StructuredFormatDisplay("{Value}")>]
    type MaxValue =
    | Unbounded
    | Bounded   of uint64
    with
        static member Make (bound : uint64) =
            if bound = 0uL then
                throw "MaxValue cannot be zero"
            Bounded bound

        static member Make (bound : uint32) =
            if bound = 0ul then
                throw "MaxValue cannot be zero"
            Bounded (uint64 bound)

        static member Make (bound : int32) =
            if bound <= 0 then
                throw "MaxValue cannot be zero or negative"
            Bounded (uint64 bound)

        member this.Value =
            match this with
            | Unbounded     -> "unbounded"
            | Bounded v     -> sprintf "%d" v

        override this.ToString() = this.Value


    [<StructuredFormatDisplay("{Value}")>]
    type Modifier =
    | InvertMatch
    with
        member this.Value = "invert-match"
        override this.ToString() = this.Value

    [<StructuredFormatDisplay("{Value}")>]
    type MinValue = | MinValue of uint32
    with
        static member Make (value : uint32) = MinValue value
        member this.Value = let (MinValue mv) = this in sprintf "%d" mv
        override this.ToString() = this.Value

    [<StructuredFormatDisplay("{Value}")>]
    type OrderedBy =
    | User
    | System
    with
        member this.Value =
            match this with
            | User      -> "user"
            | System    -> "system"
        override this.ToString() = this.Value

    /// Captures the path-arg statements from [RFC 7950, p. 205-206]
    [<AutoOpen>]
    module Path =
        [<StructuredFormatDisplay("{Value}")>]
        type PathKey = | PathKey of Up:uint16 * Node:(IdentifierReference list)
        with
            member this.IsValid =
                let (PathKey (up, nodes)) = this
                up > 0us && (List.length nodes > 0) && (nodes |> List.forall (fun node -> node.IsValid))

            member this.Value = 
                let (PathKey (up, nodes)) = this
                let up_string = if up > 0us then String.replicate (int up) "../" else "/"
                let nodes_string =
                    if nodes.Length = 0 then ""
                    else nodes |> List.map (fun n -> n.Value) |> String.concat "/"
                sprintf "%s%s" up_string nodes_string 

            override this.ToString() = this.Value

        [<StructuredFormatDisplay("{Value}")>]
        type PathPredicate = | PathPredicate of Node:IdentifierReference * PathKey:PathKey
        with
            member this.Value =
                let (PathPredicate (node, key)) = this
                sprintf "%s = current()/%s" node.Value key.Value

            override this.ToString() = this.Value

        [<StructuredFormatDisplay("{Value}")>]
        type PathItem = | PathItem of Node:IdentifierReference * Predicate:(PathPredicate option)
        with
            static member Make (identifier : string) =
                PathItem (IdentifierReference.Make identifier, None)

            member this.Value =
                let (PathItem (node, predicate)) = this
                match predicate with
                | None              -> sprintf "%s" node.Value
                | Some predicate    -> sprintf "%s[%s]" node.Value predicate.Value

            override this.ToString() = this.Value

        [<StructuredFormatDisplay("{Value}")>]
        type AbsolutePath = | AbsolutePath of PathItem list
        with
            static member Make (identifier : string) =
                AbsolutePath [ PathItem (IdentifierReference.Make identifier, None) ]
            static member Make (identifier : string list) =
                let ids = identifier |> List.map IdentifierReference.Make
                let nodes = ids |> List.map (fun n -> PathItem (n, None))
                AbsolutePath nodes

            member private this.Raw = let (AbsolutePath path) = this in path
            member this.IsValid = this.Raw.Length > 0
            member this.Value = sprintf "/%s" (this.Raw |> List.map (fun node -> node.Value) |> String.concat "/")
            override this.ToString() = this.Value

            member this.Append (identifier : string, ?predicate : PathPredicate) =
                let item = PathItem (IdentifierReference.Make identifier, predicate)
                AbsolutePath (this.Raw @ [item])

            member this.Append (identifier : IdentifierReference, ?predicate : PathPredicate) =
                let item = PathItem (identifier, predicate)
                AbsolutePath (this.Raw @ [item])

        type RelativePath = | RelativePath of Up:uint16 * PathItem list
        with
            static member Make (identifier : string, ?up : uint16) =
                let up = defaultArg up 1us
                let path = [ PathItem (IdentifierReference.Make identifier, None) ]
                RelativePath (up, path)

            static member Make (identifier : string list, ?up : uint16) =
                let up = defaultArg up 1us
                let ids = identifier |> List.map IdentifierReference.Make
                let nodes = ids |> List.map (fun n -> PathItem (n, None))
                RelativePath (up, nodes)

            member this.Path = let (RelativePath (_, path)) = this in path
            member this.UpSteps   = let (RelativePath (up, _))   = this in up

            member this.IsValid = this.Path.Length > 0 && this.UpSteps > 0us
            member this.Value =
                sprintf "%s%s"
                    (String.replicate (int this.UpSteps) "../")
                    (this.Path |> List.map (fun node -> node.Value) |> String.concat "/")

            override this.ToString() = this.Value

            member this.Append (identifier : string, ?predicate : PathPredicate) =
                let item = PathItem (IdentifierReference.Make identifier, predicate)
                RelativePath (this.UpSteps, (this.Path @ [item]))

            member this.Append (identifier : IdentifierReference, ?predicate : PathPredicate) =
                let item = PathItem (identifier, predicate)
                RelativePath (this.UpSteps, this.Path @ [item])

        type Path =
        | Absolute of AbsolutePath
        | Relative of RelativePath
        with
            member this.Value =
                match this with
                | Absolute path -> path.Value
                | Relative path -> path.Value


    /// Definition of Range ([RFC 7950, p. 204])
    [<AutoOpen>]
    module Range =

        [<StructuredFormatDisplay("{Value}")>]
        [<CustomEquality; CustomComparison>]
        type RangeBoundary =
        | Min
        | Max
        | Integer of int64
        | Decimal of System.Decimal
        with
            static member Make (value : int64) = Integer value
            static member Make (value : int32) = Integer (int64 value)
            static member Make (value : System.Decimal) = Decimal value

            member this.Value =
                match this with
                | Min       -> "min"
                | Max       -> "max"
                | Integer v -> sprintf "%d" v
                | Decimal v -> sprintf "%f" v

            override this.ToString() = this.Value

            member this._IsMin = match this with | Min -> true | _ -> false
            member this._IsMax = match this with | Max -> true | _ -> false
            member this._IsInteger = match this with | Integer _ -> true | _ -> false
            member this._IsDecimal = match this with | Decimal _ -> true | _ -> false

            member this.AsInteger =
                match this with
                | Integer value -> Some value
                | _             -> None

            member this.AsDecimal =
                match this with
                | Decimal value -> Some value
                | _             -> None

            interface IEquatable<RangeBoundary> with
                member this.Equals(other : RangeBoundary) =
                    match this, other with
                    | Min, Min
                    | Max, Max
                        -> true

                    | Min, _
                    | _, Min
                    | _, Max
                    | Max, _
                        -> false

                    | Integer v1, Integer v2    -> v1 = v2
                    | Decimal v1, Decimal v2    -> v1 = v2
                    | Integer v1, Decimal v2
                    | Decimal v2, Integer v1
                        -> (System.Decimal v1) = v2

            interface IComparable<RangeBoundary> with
                member this.CompareTo(other : RangeBoundary) =
                    match this, other with
                    | Min, Min
                    | Max, Max
                        -> 0

                    | Min, _
                    | _, Max
                        -> -1

                    | _, Min
                    | Max, _
                        -> 1

                    | Integer v1, Integer v2    -> v1.CompareTo(v2)
                    | Decimal v1, Decimal v2    -> v1.CompareTo(v2)
                    | Integer v1, Decimal v2
                    | Decimal v2, Integer v1
                        -> (System.Decimal v1).CompareTo(v2)

        [<StructuredFormatDisplay("{Value}")>]
        type RangePart =
        | Single    of RangeBoundary
        | Region    of RangeBoundary * RangeBoundary
        with
            static member Make(value : int64) = Single (RangeBoundary.Make value)
            static member Make(value : int32) = Single (RangeBoundary.Make value)
            static member Make(value : Decimal) = Single (RangeBoundary.Make value)

            static member Make(left : int64, right : int64) =
                if left > right then throw "Invalid range: %d .. %d" left right
                if left = right then Single (RangeBoundary.Make left)
                else Region (RangeBoundary.Make left, RangeBoundary.Make right)

            static member Make(left : int32, right : int32) =
                if left > right then throw "Invalid range: %d .. %d" left right
                if left = right then Single (RangeBoundary.Make left)
                else Region (RangeBoundary.Make left, RangeBoundary.Make right)

            static member Make(left : Decimal, right : Decimal) =
                if left > right then throw "Invalid range: %f .. %f" left right
                if left = right then Single (RangeBoundary.Make left)
                else Region (RangeBoundary.Make left, RangeBoundary.Make right)

            member this.Value =
                match this with
                | Single boundary       -> boundary.Value
                | Region (left, right)  -> sprintf "%s .. %s" left.Value right.Value

            override this.ToString() = this.Value

            member this._IsSingle =
                match this with
                | Single _ -> true
                | Region _ -> false

            member this._IsRegion =
                match this with
                | Single _ -> false
                | Region _ -> true

            member this.AsSingle =
                match this with
                | Single boundary   -> Some boundary
                | _                 -> None

            member this.AsRegion =
                match this with
                | Single _              -> None
                | Region (left, right)  -> Some (left, right)

            member this.IsInRange (value : int64) =
                match this with
                | Single Min
                | Single Max    -> false
                | Single (Integer v)    -> value = v
                | Single (Decimal v)    -> (System.Decimal value) = v
                | Region (_, Min)
                | Region (Max, _)       -> throw "Invalid range-part: %A" this
                | Region (Min, Max)     ->
                    _logger.Warn("Detected trivial range part (min .. max)")
                    true
                | Region (Min, Integer v)   -> value <= v
                | Region (Min, Decimal v)   -> (System.Decimal value) <= v
                | Region (Integer v, Max)   -> v <= value
                | Region (Decimal v, Max)   -> v <= (System.Decimal value)
                | Region (Integer left, Integer right) ->
                    left <= value && value <= right
                | Region (Decimal left, Decimal right) ->
                    let value' = System.Decimal value
                    left <= value' && value' <= right
                | Region (Integer left, Decimal right) ->
                    let value' = System.Decimal value
                    left <= value && value' <= right
                | Region (Decimal left, Integer right) ->
                    let value' = System.Decimal value
                    left <= value' && value <= right

            member this.IsInRange (value : int32) = this.IsInRange (int64 value)

            member this.IsInRange (value : Decimal) =
                match this with
                | Single Min
                | Single Max    -> false
                | Single (Integer v)    -> value = (System.Decimal v)
                | Single (Decimal v)    -> value = v
                | Region (_, Min)
                | Region (Max, _)       -> throw "Invalid range-part: %A" this
                | Region (Min, Max)     ->
                    _logger.Warn("Detected trivial range part (min .. max)")
                    true
                | Region (Min, Integer v)   -> value <= (System.Decimal v)
                | Region (Min, Decimal v)   -> value <= v
                | Region (Integer v, Max)   -> (System.Decimal v) <= value
                | Region (Decimal v, Max)   -> v <= value
                | Region (Integer left, Integer right) ->
                    (System.Decimal left) <= value && value <= (System.Decimal right)
                | Region (Decimal left, Decimal right) ->
                    left <= value && value <= right
                | Region (Integer left, Decimal right) ->
                    (System.Decimal left) <= value && value <= right
                | Region (Decimal left, Integer right) ->
                    left <= value && value <= (System.Decimal right)

            member this.IsValid =
                match this with
                | Single _  -> true
                | Region (left, right) ->
                    (left :> IComparable<RangeBoundary>).CompareTo(right) <= 0


        /// Definition of Range ([RFC 7950, p. 204])
        [<StructuredFormatDisplay("{Value}")>]
        type Range = | Range of RangePart list
        with
            static member Make (left : int64, right : int64) =
                if left > right then throw "Left region cannot be larger than right (%d .. %d)" left right
                if left = right then
                    Range [ Single (Integer left) ]
                else
                    Range [ Region (Integer left, Integer right) ]

            static member Make (left : int32, right : int32) =
                if left > right then throw "Left region cannot be larger than right (%d .. %d)" left right
                if left = right then
                    Range [ Single (Integer (int64 left)) ]
                else
                    Range [ Region (Integer (int64 left), Integer (int64 right)) ]

            static member Make (left : System.Decimal, right : System.Decimal) =
                if left > right then throw "Left region cannot be larger than right (%f .. %f)" left right
                if left = right then
                    Range [ Single (Decimal left) ]
                else
                    Range [ Region (Decimal left, Decimal right) ]

            member this.Value = let (Range range) = this in range |> List.map (fun part -> part.Value) |> String.concat " | "
            override this.ToString() = this.Value

            member this.IsInRange (value : int64) =
                let (Range range) = this
                range |> List.exists (fun part -> part.IsInRange value)

            member this.IsInRange (value : int32) =
                let (Range range) = this
                range |> List.exists (fun part -> part.IsInRange value)

            member this.IsInRange (value : Decimal) =
                let (Range range) = this
                range |> List.exists (fun part -> part.IsInRange value)

            member this.IsValid =
                // TODO: Implement unit tests for validity check for range, see [RFC 7950, p. 147]
                let (Range ranges) = this
                let is_valid, _ =
                    ranges |> List.fold (
                        fun (valid, minimum) range ->
                            if valid = false            then (false, None)
                            elif range.IsValid = false  then (false, None)
                            else
                                match range with
                                | Single r ->
                                    if minimum.IsNone then (true, Some r)
                                    elif (minimum.Value :> IComparable<RangeBoundary>).CompareTo(r) < 0 then (true, Some r)
                                    else (false, None)
                                | Region (left, right) ->
                                    // We have already checked that the range is valid above.
                                    if (minimum.Value :> IComparable<RangeBoundary>).CompareTo(left) < 0 then (true, Some right)
                                    else (false, None)
                    ) (true, None)

                is_valid

            member this.Append (part : RangePart) =
                // TODO: Implement range narrowing, see [RFC 7950, p. 147]
                let (Range ranges) = this
                throw "Range narrowing not implemented"


        let MakeInt32Range (left : int32, right : int32) = Range.Make (left, right)


    [<StructuredFormatDisplay("{Value}")>]
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

        override this.ToString() = this.Value

    /// Set of descendant schema nodes that specify uniqueness
    [<StructuredFormatDisplay("{Value}")>]
    type Unique = | Unique of SchemaNodeIdentifier list
    with
        static member Make (schema : SchemaNodeIdentifier list) =
            if (List.length schema) = 0 then
                throw "Cannot create a unique-arg from an empty list"
            if (schema |> List.exists (fun s -> s.IsAbsolute)) then
                throw "Cannot create a unique-arg from absolute paths"

            Unique schema

        member this.Value =
            let (Unique schema) = this
            schema |> List.map (fun i -> i.Value) |> String.concat " "
        override this.ToString() = this.Value

    let MakeUnique = Unique.Make
    let MakeUniqueDescendant (schema : SchemaNodeIdentifier list ) =
        if (schema |> List.exists (fun s -> s.IsAbsolute)) then
            throw "Did not expect absolute path when creating a unique arg"
        MakeUnique schema


    /// Captures the 'augment-arg' definition ([RFC 7950, p. 199])
    type Augment = SchemaNodeIdentifier

    /// Captures the 'deviation-arg' definition ([RFC 7950, p. 201])
    type Deviation = SchemaNodeIdentifier

    /// Captures the 'refine-arg' definition ([RFC 7950, p. 198])
    type Refine = SchemaNodeIdentifier

    /// Captures the 'uses-augment-arg' definition ([RFC 7950, p. 198])
    type UsesAugment = SchemaNodeIdentifier

    let BoolAsString v = if v then "true" else "false"

