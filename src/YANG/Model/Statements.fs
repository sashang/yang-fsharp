// Statenents.fs
namespace Yang.Model

/// Definition of Yang statements
[<AutoOpen>]
module Statements =
    open System
    open Identifier

    type Status =
    | Current
    | Obsolete
    | Deprecated

    /// Available Yang statement definitions
    [<StructuredFormatDisplay("{PrettyPrint}")>]
    type Statement =
    // Observe that many rules can be followed by blocks of unknown statements.
    // We store the statements in those blocks in a special field called Options.
    // Typically this should be empty.

    | BelongsTo     of BelongsToStatement
    | Contact       of ContactStatement
    | Description   of DescriptionStatement
    | ErrorAppTag   of ErrorAppTagStatement
    | ErrorMessage  of ErrorMessageStatement
    | Extension     of ExtensionStatement
    | Import        of ImportStatement
    | Include       of IncludeStatement
    | Namespace     of NamespaceStatement
    | Organization  of OrganizationStatement
    | Prefix        of PrefixStatement
    | Presence      of PresenceStatement
    | Reference     of ReferenceStatement
    | Revision      of RevisionStatement
    | RevisionDate  of RevisionDateStatement
    | Status        of StatusStatement
    | Units         of UnitsStatement
    | YangVersion   of YangVersionStatement
    | YinElement    of YinElementStatement
    | Unknown       of UnknownStatement
    | Unparsed      of Identifier:Identifier * Argument:(string option) * Body:(Statement list option)
    with
        member this.Options =
            match this with
            | Contact       (_, options)
            | Description   (_, options)
            | ErrorAppTag   (_, options)
            | ErrorMessage  (_, options)
            | Namespace     (_, options)
            | Organization  (_, options)
            | Prefix        (_, options)
            | Presence      (_, options)
            | Reference     (_, options)
            | YangVersion   (_, options)
                -> options
            | Unknown _
            | Unparsed _
                -> None

        member this.Identifier =
            match this with
            | Contact _         -> "contact"
            | Description _     -> "description"
            | ErrorAppTag _     -> "error-app-tag"
            | ErrorMessage _    -> "error-message"
            | Namespace _       -> "namespace"
            | Organization _    -> "organization"
            | Prefix _          -> "prefix"
            | Presence _        -> "presence"
            | Reference _       -> "reference"
            | YangVersion _     -> "yang-version"
            | Unknown (id, _, _)    -> id.ToString()
            | Unparsed (id, _, _)   -> id.ToString()

        member this.PrettyPrint =
            let escape = [| ' '; '\t'; '\r'; '\n'; ';'; '{'; '}'; '@'; ':' |]

            /// Pretty print string
            let ps (input : string) =
                if input.IndexOfAny(escape) > 0 then sprintf "\"%s\"" input
                else input

            /// Pretty print optional string
            let pso (input : string option) =
                match input with
                | Some str ->
                    if str.IndexOfAny(escape) > 0 then sprintf "\"%s\"" str
                    else str
                | None -> ""

            /// Pretty print block
            let pb (options : Statement list option) =
                match options with
                | None -> ""
                | Some block -> sprintf "{ %A }" block

            let id = this.Identifier

            match this with
            | Contact (s, b)
            | Description (s, b)
            | ErrorAppTag (s, b)
            | ErrorMessage (s, b)
            | Organization (s, b)
            | Prefix (s, b)
            | Presence (s, b)
            | Reference (s, b)
                -> sprintf "%s %s %s" id (ps s) (pb b)

            | Namespace (uri, block)        -> sprintf "%s %s %s" id (uri.ToString()) (pb block)
            | YangVersion (version, block)  -> sprintf "%s %s %s" id (version.ToString()) (pb block)

            | Unknown (id, arg, body)       -> sprintf "Unknown: %A %s %s"  id (pso arg) (pb body)
            | Unparsed (id, arg, body)      -> sprintf "Unparsed: %A %s %s" id (pso arg) (pb body)

    /// Short name for the extra statements that may appear
    and ExtraStatements         = Statement list option

    and ArgumentBodyStatement   =
    | YinElement    of YinElementStatement
    | Unknown       of UnknownStatement
    and ArgumentStatement       = Identifier * (ArgumentBodyStatement list option)
    and BelongsToBodyStatement  =
    | Prefix        of PrefixStatement
    and BelongsToStatement      = Identifier * (BelongsToBodyStatement list)
    and ContactStatement        = string  * ExtraStatements
    and DescriptionStatement    = string  * ExtraStatements
    and ErrorAppTagStatement    = string  * ExtraStatements
    and ErrorMessageStatement   = string  * ExtraStatements
    and ExtensionBodyStatement  =
    | Argument      of ArgumentStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and ExtensionStatement      = Identifier * (ExtensionBodyStatement list option)
    and ImportBodyStatement     =
    | Prefix        of PrefixStatement
    | RevisionDate  of RevisionDateStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and ImportStatement         = Identifier * (ImportBodyStatement list)
    and IncludeBodyStatement    =
    | RevisionDate  of RevisionDateStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and IncludeStatement        = Identifier * (ImportBodyStatement list option)
    and NamespaceStatement      = Uri     * ExtraStatements
    and OrganizationStatement   = string  * ExtraStatements
    and PrefixStatement         = string  * ExtraStatements
    and PresenceStatement       = string  * ExtraStatements
    and ReferenceStatement      = string  * ExtraStatements
    and RevisionBodyStatement   =
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and RevisionStatement       = Tokens.Date   * (RevisionBodyStatement list option)
    and RevisionDateStatement   = Tokens.Date   * ExtraStatements
    and StatusStatement         = Status        * ExtraStatements
    and UnitsStatement          = string        * ExtraStatements
    and YangVersionStatement    = Version       * ExtraStatements
    and YinElementStatement     = bool          * ExtraStatements

    and UnknownStatement        = IdentifierWithPrefix * (string option) * ExtraStatements
