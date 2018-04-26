// Identifier.fs

namespace Yang.Model

/// Parsers and types for YANG identifiers
[<AutoOpen>]
module Identifier =
    open System
    open System.Text
    open NLog
    open System.Runtime.InteropServices

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

    let private isAsciiIdStart c = Char.IsLetter(c) || c = '_'
    let private isAsciiIdContinue c =
        Char.IsLetterOrDigit(c) || c = '_' || c = '-' || c = '.'

    /// Checks whether a string is a valid identifier name
    let is_identifier_valid (input : string) =
        System.String.IsNullOrWhiteSpace(input) = false &&
        isAsciiIdStart (input.Chars 0) &&
        (String.forall isAsciiIdContinue input)

    /// YANG Identifier
    [<StructuredFormatDisplay("{Value}")>]
    [<Struct>]
    type Identifier = private String of string
    with
        /// <summary>
        /// Creates an identifier from the input string,
        /// without checking whether the string is valid.
        /// The caller should guarantee validity of the input.
        /// </summary>
        /// <param name="name">The identifier</param>
        static member MakeUnchecked (name : string) = String name

        /// <summary>
        /// Creates an identifier from the input string,
        /// </summary>
        /// <param name="name">The input identifier</param>
        static member Make (name : string) =
            if (is_identifier_valid name) = false then
                throw "Invalid identifier: %s" name
            else
                String name

        /// <summary>
        /// Gets the string value of the identifier
        /// </summary>
        member this.Value = let (String value) = this in value

        /// <summary>
        /// Checks whether the identifier has a valid name
        /// </summary>
        member this.IsValid = is_identifier_valid this.Value

        /// Checks whether this is an identifier of a well known type.
        member this.IsBuiltIn =
            let (String value) = this
            String.Equals(value, "binary", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "bits", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "boolean", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "decimal64", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "empty", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "enumeration", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "identityref", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "instance-identifier", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "int8", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "int16", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "int32", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "int64", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "leafref", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "string", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "uint8", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "uint16", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "uint32", StringComparison.InvariantCultureIgnoreCase) ||
            String.Equals(value, "uint64", StringComparison.InvariantCultureIgnoreCase)

        override this.ToString() = this.Value

    /// YANG Identifier with prefix
    [<StructuredFormatDisplay("{Value}")>]
    [<Struct>]
    type IdentifierWithPrefix = IdentifierWithPrefix of Prefix:string * Name:String
    with
        /// <summary>
        /// Creates a composite identifier without checking validity of input string;
        /// Caller should guarantee that the prefix and name are valid.
        /// </summary>
        /// <param name="prefix">The prefix of the identifier</param>
        /// <param name="name">The name of the identifier</param>
        static member MakeUnchecked (prefix, name) = (IdentifierWithPrefix (prefix, name))

        /// <summary>
        /// Creates a composite identifier
        /// </summary>
        /// <param name="prefix">The prefix of the identifier</param>
        /// <param name="name">The name of the identifier</param>
        static member Make (prefix, name) =
            if (is_identifier_valid prefix) = false then
                throw "Invalid prefix for identifier: %s" prefix
            if (is_identifier_valid name) = false then
                throw "Invalid name of (prefixed) identifier: %s" name

            (IdentifierWithPrefix (prefix, name))

        /// <summary>
        /// Create a composite identifier
        /// </summary>
        /// <param name="id">The identifier as a string</param>
        static member Make (id) =
            if String.IsNullOrWhiteSpace(id) then
                throw "Cannot create key from empty string"

            let separator = id.IndexOf(':')
            if separator < 0 then
                throw "Invalid name for custom identifier (missing colon ':'): %s" id

            let rseparator = id.IndexOf(':')
            if separator <> rseparator then
                throw "Invalid name for custom identifier (multiple prefixes): %s" id

            let prefix      = id.Substring(0, separator)
            let identifier  = id.Substring(separator+1)
            IdentifierWithPrefix.Make(prefix, identifier)

        member this._Prefix = let (IdentifierWithPrefix (prefix, _)) = this in prefix
        member this._Name   = let (IdentifierWithPrefix (_, name))   = this in name

        /// <summary>
        /// Gets the string value of the identifier
        /// </summary>
        member this.Value = sprintf "%s:%s" this._Prefix this._Name

        /// <summary>
        /// Checks whether the identifier has a valid name
        /// </summary>
        member this.IsValid = (is_identifier_valid this._Prefix) && (is_identifier_valid this._Name)

        override this.ToString() = this.Value

    /// Captures either a simple or custom identifier
    [<StructuredFormatDisplay("{Value}")>]
    [<Struct>]
    type IdentifierReference =
    | Simple of SimpleId:Identifier
    | Custom of PrefixedId:IdentifierWithPrefix
    with
        /// <summary>
        /// Create an identifier from a string
        /// </summary>
        /// <param name="id">The identifier</param>
        static member Make (id : string) =
            if String.IsNullOrWhiteSpace(id) then
                throw "Cannot create key from empty string"

            if id.IndexOf(':') < 0 then
                Simple (Identifier.Make id)
            else
                Custom (IdentifierWithPrefix.Make id)

        /// <summary>
        /// Create a reference identifier from a simple identifier
        /// </summary>
        /// <param name="id">The input identifier</param>
        static member Make (id : Identifier)            = Simple id

        /// <summary>
        /// Create a reference identifier from a prefixed identifier
        /// </summary>
        /// <param name="id">The input identifier</param>
        static member Make (id : IdentifierWithPrefix)  = Custom id

        member this._IsSimple = match this with | Simple _ -> true | _ -> false
        member this._IsCustom = match this with | Custom _ -> true | _ -> false

        member this.AsSimple =
            match this with
            | Simple id     -> Some id
            | _             -> None

        member this.AsCustom =
            match this with
            | Custom id     -> Some id
            | _id           -> None

        /// <summary>
        /// Get the string representation of the identifier
        /// </summary>
        member this.Value =
            match this with
            | Simple identifier -> identifier.Value
            | Custom identifier -> identifier.Value

        /// Checks whether this is an identifier of a well known type.
        member this.IsPrimitive =
            match this with
            | Simple id -> id.IsBuiltIn
            | _         -> false

        override this.ToString() = this.Value

        /// <summary>
        /// Checks whether the identifier is valid
        /// </summary>
        member this.IsValid =
            match this with
            | Simple identifier -> identifier.IsValid
            | Custom identifier -> identifier.IsValid

    [<StructuredFormatDisplay("{Value}")>]
    [<Struct>]
    type SchemaNodeIdentifier = SchemaNodeIdentifier of Schema:(IdentifierReference list) * Absolute:bool
    with
        static member MakeAbsolute (schema : IdentifierReference list) =
            if schema.Length = 0 then
                throw "Empty list of identifiers when creating schema"
            SchemaNodeIdentifier (schema, true)

        static member MakeDescendant (schema : IdentifierReference list) =
            if schema.Length = 0 then
                throw "Empty list of identifiers when creating schema"
            SchemaNodeIdentifier (schema, false)

        member this.IsAbsolute = let (SchemaNodeIdentifier (_, absolute)) = this in absolute
        member this.IsDescendant = let (SchemaNodeIdentifier (_, absolute)) = this in absolute = false
        member this._Schema = let (SchemaNodeIdentifier (schema, _)) = this in schema
        member this.Item index = let (SchemaNodeIdentifier (schema, _)) = this in List.item index schema

        member this.Value =
            let (SchemaNodeIdentifier (schema, absolute)) = this
            let prefix = if absolute then "/" else ""
            sprintf "%s%s" prefix (schema |> List.map (fun i -> i.Value) |> String.concat "/")

    let private to_yang_double_quoted_string (input : string) =
        input.Replace("\\", "\\\\").
              Replace("\"", "\\\"").
              Replace("\n", "\\n").
              Replace("\t", "\\t")

    [<StructuredFormatDisplay("{Value}")>]
    [<Struct>]
    type KeyPredicateExpression = KeyPredicateExpression of IdentifierReference * string
    with
        member this.Identifier = let (KeyPredicateExpression (id, _)) = this in id

        member this.Value =
            let (KeyPredicateExpression (identifier, expression)) = this
            let sb = StringBuilder()
            Printf.bprintf sb "%s = " (identifier.Value)
            if expression.Contains("'") = false then
                // Easy case: the string does not contain a single quote,
                // hence it is safe to wrap it in single quotes without any change
                Printf.bprintf sb "'%s'" expression
            else
                let expression' = to_yang_double_quoted_string expression
                Printf.bprintf sb "\"%s\"" expression'

            sb.ToString()

        override this.ToString() = this.Value

    [<StructuredFormatDisplay("{Value}")>]
    [<Struct>]
    type InstanceIdentifierFilter =
    | KeyPredicate      of Key:KeyPredicateExpression
    | LeafListPredicate of LeafList:string
    | Position          of Pos:uint32
    with
        member this.Value =
            match this with
            | KeyPredicate key      -> key.Value
            | LeafListPredicate ll  -> sprintf ". = %s" (to_yang_double_quoted_string ll)
            | Position pos          -> sprintf "%d" pos

        override this.ToString() = this.Value

    [<StructuredFormatDisplay("{Value}")>]
    [<Struct>]
    type InstanceIdentifierNode = InstanceIdentifierNode of IdentifierReference * (InstanceIdentifierFilter option)
    with
        member this.ToStringBuilder (sb : StringBuilder) =
            let (InstanceIdentifierNode (id, filter)) = this
            match filter with
            | None          -> Printf.bprintf sb "%s" id.Value
            | Some filter   -> Printf.bprintf sb "%s[%s]" id.Value filter.Value

        member this.Value =
            let (InstanceIdentifierNode (id, filter)) = this
            match filter with
            | None          -> sprintf "%s" id.Value
            | Some filter   -> sprintf "%s[%s]" id.Value filter.Value

        override this.ToString() = this.Value

    [<StructuredFormatDisplay("{Value}")>]
    [<Struct>]
    type InstanceIdentifier = InstanceIdentifier of InstanceIdentifierNode list
    with
        member this.Nodes = let (InstanceIdentifier nodes) = this in nodes

        member this.ToStringBuilder(sb : StringBuilder) =
            this.Nodes |> List.iter (
                fun node ->
                    Printf.bprintf sb "/"
                    node.ToStringBuilder sb
            )

        member this.Value =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.Value
