// Statenents.fs
namespace Yang.Model

/// Definition of Yang statements
[<AutoOpen>]
module Statements =
    open System
    open Arguments

    /// Available Yang statement definitions
    [<StructuredFormatDisplay("{PrettyPrint}")>]
    type Statement =
    // Observe that many rules can be followed by blocks of unknown statements.
    // We store the statements in those blocks in a special field called Options.
    // Typically this should be empty.

    | Argument      of ArgumentStatement
    | Base          of BaseStatement
    | BelongsTo     of BelongsToStatement
    | Config        of ConfigStatement
    | Contact       of ContactStatement
    | Description   of DescriptionStatement
    | ErrorAppTag   of ErrorAppTagStatement
    | ErrorMessage  of ErrorMessageStatement
    | Extension     of ExtensionStatement
    | Feature       of FeatureStatement
    | Identity      of IdentityStatement
    | IfFeature     of IfFeatureStatement
    | Import        of ImportStatement
    | Include       of IncludeStatement
    | Key           of KeyStatement
    | Mandatory     of MandatoryStatement
    | MaxElements   of MaxElementsStatement
    | MinElements   of MinElementsStatement
    | Must          of MustStatement
    | Namespace     of NamespaceStatement
    | OrderedBy     of OrderedByStatement
    | Organization  of OrganizationStatement
    | Position      of PositionStatement
    | Prefix        of PrefixStatement
    | Presence      of PresenceStatement
    | Range         of RangeStatement
    | Reference     of ReferenceStatement
    | Revision      of RevisionStatement
    | RevisionDate  of RevisionDateStatement
    | Status        of StatusStatement
    | Units         of UnitsStatement
    | Value         of ValueStatement
    | YangVersion   of YangVersionStatement
    | YinElement    of YinElementStatement
    | Unknown       of UnknownStatement
    | Unparsed      of Identifier:Identifier * Argument:(string option) * Body:(Statement list option)
    with
        /// Retrieves the extra options that may appear at the end of the statement;
        /// If the statement has a statement specific body, then this call will return None,
        /// and the caller will need to apply per-statement processing to retrieve those statements.
        member this.Options =
            match this with
            | Base          (_, options)
            | Config        (_, options)
            | Contact       (_, options)
            | Description   (_, options)
            | ErrorAppTag   (_, options)
            | ErrorMessage  (_, options)
            | IfFeature     (_, options)
            | Key           (_, options)
            | Mandatory     (_, options)
            | MaxElements   (_, options)
            | MinElements   (_, options)
            | Namespace     (_, options)
            | OrderedBy     (_, options)
            | Organization  (_, options)
            | Position      (_, options)
            | Prefix        (_, options)
            | Presence      (_, options)
            | Reference     (_, options)
            | RevisionDate  (_, options)
            | Status        (_, options)
            | Units         (_, options)
            | Value         (_, options)
            | YangVersion   (_, options)
            | YinElement    (_, options)
                -> options

            // The following have custom options; the caller need to treat them specially
            | Argument      (_, _)
            | BelongsTo     (_, _)
            | Extension     (_, _)
            | Feature       (_, _)
            | Identity      (_, _)
            | Import        (_, _)
            | Include       (_, _)
            | Must          (_, _)
            | Range         (_, _)
            | Revision      (_, _)
                -> None

            | Unknown _
            | Unparsed _
                -> None

        member this.Identifier =
            match this with
            | Argument _        -> "argument"
            | Base _            -> "base"
            | BelongsTo _       -> "belongs-to"
            | Config _          -> "config"
            | Contact _         -> "contact"
            | Description _     -> "description"
            | ErrorAppTag _     -> "error-app-tag"
            | ErrorMessage _    -> "error-message"
            | Extension _       -> "extension"
            | Feature _         -> "feature"
            | Identity _        -> "identity"
            | IfFeature _       -> "if-feature"
            | Import _          -> "import"
            | Include _         -> "include"
            | Key _             -> "key"
            | Mandatory _       -> "mandatory"
            | MaxElements _     -> "max-elements"
            | MinElements _     -> "min-elements"
            | Must _            -> "must"
            | Namespace _       -> "namespace"
            | OrderedBy _       -> "ordered-by"
            | Organization _    -> "organization"
            | Position _        -> "position"
            | Prefix _          -> "prefix"
            | Presence _        -> "presence"
            | Range _           -> "range"
            | Reference _       -> "reference"
            | Revision _        -> "revision"
            | RevisionDate _    -> "revision-date"
            | Status _          -> "status"
            | Units _           -> "units"
            | Value _           -> "value"
            | YangVersion _     -> "yang-version"
            | YinElement _      -> "yin-element"
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
    and ArgumentStatement       = Identifier            * (ArgumentBodyStatement list option)
    and BaseStatement           = IdentifierReference   * ExtraStatements
    and BelongsToBodyStatement  =
    | Prefix        of PrefixStatement
    | Unknown       of UnknownStatement
    and BelongsToStatement      = Identifier    * (BelongsToBodyStatement list)
    and ConfigStatement         = bool          * ExtraStatements
    and ContactStatement        = string        * ExtraStatements
    and DescriptionStatement    = string        * ExtraStatements
    and ErrorAppTagStatement    = string        * ExtraStatements
    and ErrorMessageStatement   = string        * ExtraStatements
    and ExtensionBodyStatement  =
    | Argument      of ArgumentStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and ExtensionStatement      = Identifier            * (ExtensionBodyStatement list option)
    and FeatureBodyStatement    =
    | IfFeature     of IfFeatureStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    and FeatureStatement        = Identifier            * (FeatureBodyStatement list option)
    and IdentityBodyStatement   =
    | IfFeature     of IfFeatureStatement
    | Base          of BaseStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and IdentityStatement       = Identifier            * (IdentityBodyStatement list option)
    and IfFeatureStatement      = IfFeatureExpression   * ExtraStatements
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
    and IncludeStatement        = Identifier    * (IncludeBodyStatement list option)
    and KeyStatement            = Key           * ExtraStatements
    and MandatoryStatement      = bool          * ExtraStatements
    and MaxElementsStatement    = MaxValue      * ExtraStatements
    and MinElementsStatement    = uint32        * ExtraStatements
    and MustBodyStatement       =
    | ErrorMessage  of ErrorMessageStatement
    | ErrorAppTag   of ErrorAppTagStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and MustStatement           = string        * (MustBodyStatement list option)
    and NamespaceStatement      = Uri           * ExtraStatements
    and OrderedByStatement      = OrderedBy     * ExtraStatements
    and OrganizationStatement   = string        * ExtraStatements
    and PositionStatement       = uint32        * ExtraStatements
    and PrefixStatement         = string        * ExtraStatements
    and PresenceStatement       = string        * ExtraStatements
    and RangeBodyStatement      =
    | ErrorMessage  of ErrorMessageStatement
    | ErrorAppTag   of ErrorAppTagStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    and RangeStatement          = Range     * (RangeBodyStatement list option)
    and ReferenceStatement      = string    * ExtraStatements
    and RevisionBodyStatement   =
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and RevisionStatement       = Arguments.Date    * (RevisionBodyStatement list option)
    and RevisionDateStatement   = Arguments.Date    * ExtraStatements
    and StatusStatement         = Status            * ExtraStatements
    and UnitsStatement          = string            * ExtraStatements
    and ValueStatement          = int64             * ExtraStatements
    and WhenBodyStatement       =
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and WhenStatement           = string            * (WhenBodyStatement list option)
    and YangVersionStatement    = Version           * ExtraStatements
    and YinElementStatement     = bool              * ExtraStatements

    and UnknownStatement        = IdentifierWithPrefix * (string option) * ExtraStatements
