// Identifier.fs

namespace Yang.Model

/// Parsers and types for YANG identifiers
[<AutoOpen>]
module Identifier =
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
    type Identifier = private | String of string
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

        override this.ToString() = this.Value

    /// YANG Identifier with prefix
    [<StructuredFormatDisplay("{Value}")>]
    type IdentifierWithPrefix = {
        Prefix  : string
        Name    : string
    }
    with
        /// <summary>
        /// Creates a composite identifier without checking validity of input string;
        /// Caller should guarantee that the prefix and name are valid.
        /// </summary>
        /// <param name="prefix">The prefix of the identifier</param>
        /// <param name="name">The name of the identifier</param>
        static member MakeUnchecked (prefix, name) = { Prefix = prefix; Name = name }

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

            { Prefix = prefix; Name = name }

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

        /// <summary>
        /// Gets the string value of the identifier
        /// </summary>
        member this.Value = sprintf "%s:%s" this.Prefix this.Name

        /// <summary>
        /// Checks whether the identifier has a valid name
        /// </summary>
        member this.IsValid = (is_identifier_valid this.Prefix) && (is_identifier_valid this.Name)

        override this.ToString() = this.Value

    /// Captures either a simple or custom identifier
    [<StructuredFormatDisplay("{Value}")>]
    type IdentifierReference =
    | Simple of Identifier
    | Custom of IdentifierWithPrefix
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

        override this.ToString() = this.Value

        /// <summary>
        /// Checks whether the identifier is valid
        /// </summary>
        member this.IsValid =
            match this with
            | Simple identifier -> identifier.IsValid
            | Custom identifier -> identifier.IsValid

    [<StructuredFormatDisplay("{Value}")>]
    type SchemaNodeIdentifier = | SchemaNodeIdentifier of Schema:(IdentifierReference list) * Absolute:bool
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


