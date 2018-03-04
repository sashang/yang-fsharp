﻿// Statenents.fs
namespace Yang.Model

/// Definition of Yang statements
[<AutoOpen>]
module Statements =
    open System
    open Arguments

    (* The following captures the main part of the YANG model.
     * It tries to be reasonable close to [RFC 7950, Section 14, pp.184-210].
     * There are a few differences though that aim to reduce the complexity of the model below,
     * and to create a homogeneous syntax.
     *
     * First, even rules that do not specify custom bodies, can be followed by blocks of unknown statements
     * (they end with the 'stmtend' rule, which depends on 'stmtsep' and, as a result, 'unknown-statement').
     * This enables various extensions. We store the statements in those blocks in a special type called
     * ExtraOptions, which follows the main definition. in most cases, we expect that to be empty (None).
     *
     * Furthermore, the 'unknown-statement' [RFC 7950, p. 202] can include a body which contains
     * the 'yang-stmt' rule, which basically captures all available statements. This is why almost every statement
     * depends on every other, and, hence they need to be defined together.
     *
     * Second, there can be arbitrary 'unknown-statement' definitions between any pair of known statements.
     * This is due to the 'stmtsep' rule. Hence, it makes it easier to use lists to specify the various valid
     * blocks (in most cases, there is no ordering imposed on those statements, hence the choice of list is
     * reasonable).
     *
     * The model requires in many cases that the cardinality of a particular statement in a block is one or more.
     * It is not particularly convenient to define lists that have at least one item, and we instead use lists.
     * The caller/user needs to verify cardinality --- in other words, cardinality constraints are not enforced
     * below.
     *)

    /// Available Yang statement definitions
    [<StructuredFormatDisplay("{PrettyPrint}")>]
    type Statement =
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Argument      of ArgumentStatement
    | Base          of BaseStatement
    | BelongsTo     of BelongsToStatement
    | Bit           of BitStatement
    | Config        of ConfigStatement
    | Contact       of ContactStatement
    | Default       of DefaultStatement
    | Description   of DescriptionStatement
    | DeviateAdd    of DeviateAddStatement
    | DeviateDelete of DeviateDeleteStatement
    | DeviateReplace        of DeviateReplaceStatement
    | DeviateNotSupported   of DeviateNotSupportedStatement
    | Deviation     of DeviationStatement
    | Enum          of EnumStatement
    | ErrorAppTag   of ErrorAppTagStatement
    | ErrorMessage  of ErrorMessageStatement
    | Extension     of ExtensionStatement
    | Feature       of FeatureStatement
    | FractionDigits of FractionDigitsStatement
    | Identity      of IdentityStatement
    | IfFeature     of IfFeatureStatement
    | Import        of ImportStatement
    | Include       of IncludeStatement
    | Key           of KeyStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | Length        of LengthStatement
    | Mandatory     of MandatoryStatement
    | MaxElements   of MaxElementsStatement
    | MinElements   of MinElementsStatement
    | Modifier      of ModifierStatement
    | Must          of MustStatement
    | Namespace     of NamespaceStatement
    | OrderedBy     of OrderedByStatement
    | Organization  of OrganizationStatement
    | Path          of PathStatement
    | Pattern       of PatternStatement
    | Position      of PositionStatement
    | Prefix        of PrefixStatement
    | Presence      of PresenceStatement
    | Range         of RangeStatement
    | Reference     of ReferenceStatement
    | Refine        of RefineStatement
    | Revision      of RevisionStatement
    | RevisionDate  of RevisionDateStatement
    | RequireInstance of RequireInstanceStatement
    | Status        of StatusStatement
    | Type          of TypeStatement
    | TypeDef       of TypeDefStatement
    | Unique        of UniqueStatement
    | Units         of UnitsStatement
    | Value         of ValueStatement
    | When          of WhenStatement
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
            | Default       (_, options)
            | Description   (_, options)
            | DeviateNotSupported options
            | ErrorAppTag   (_, options)
            | ErrorMessage  (_, options)
            | FractionDigits (_, options)
            | IfFeature     (_, options)
            | Key           (_, options)
            | Mandatory     (_, options)
            | MaxElements   (_, options)
            | MinElements   (_, options)
            | Modifier      (_, options)
            | Namespace     (_, options)
            | OrderedBy     (_, options)
            | Organization  (_, options)
            | Path          (_, options)
            | Position      (_, options)
            | Prefix        (_, options)
            | Presence      (_, options)
            | Reference     (_, options)
            | RevisionDate  (_, options)
            | RequireInstance (_, options)
            | Status        (_, options)
            | Units         (_, options)
            | Unique        (_, options)
            | Value         (_, options)
            | YangVersion   (_, options)
            | YinElement    (_, options)
                -> options

            // The following have custom options; the caller need to treat them specially
            | AnyData       (_, _)
            | AnyXml        (_, _)
            | Argument      (_, _)
            | BelongsTo     (_, _)
            | Bit           (_, _)
            | DeviateAdd     _
            | DeviateDelete  _
            | DeviateReplace _
            | Deviation     (_, _)
            | Enum          (_, _)
            | Extension     (_, _)
            | Feature       (_, _)
            | Identity      (_, _)
            | Import        (_, _)
            | Include       (_, _)
            | Leaf          (_, _)
            | LeafList      (_, _)
            | Length        (_, _)
            | Must          (_, _)
            | Pattern       (_, _)
            | Range         (_, _)
            | Refine        (_, _)
            | Revision      (_, _)
            | Type          (_, _, _)
            | TypeDef       (_, _)
            | When          (_, _)
                -> None

            | Unknown _
            | Unparsed _
                -> None

        member this.Identifier =
            match this with
            | AnyData _         -> "anydata"
            | AnyXml _          -> "anyxml"
            | Argument _        -> "argument"
            | Base _            -> "base"
            | BelongsTo _       -> "belongs-to"
            | Bit _             -> "bit"
            | Config _          -> "config"
            | Contact _         -> "contact"
            | Default _         -> "default"
            | Description _     -> "description"
            | DeviateAdd _      -> "deviate add"
            | DeviateDelete _   -> "deviate delete"
            | DeviateNotSupported _     -> "deviate not-supported"
            | DeviateReplace _  -> "deviate replace"
            | Deviation _       -> "deviation"
            | Enum _            -> "enum"
            | ErrorAppTag _     -> "error-app-tag"
            | ErrorMessage _    -> "error-message"
            | Extension _       -> "extension"
            | Feature _         -> "feature"
            | FractionDigits _  -> "fraction-digits"
            | Identity _        -> "identity"
            | IfFeature _       -> "if-feature"
            | Import _          -> "import"
            | Include _         -> "include"
            | Key _             -> "key"
            | Leaf _            -> "leaf"
            | LeafList _        -> "leaf-list"
            | Length _          -> "length"
            | Mandatory _       -> "mandatory"
            | MaxElements _     -> "max-elements"
            | MinElements _     -> "min-elements"
            | Modifier _        -> "modifier"
            | Must _            -> "must"
            | Namespace _       -> "namespace"
            | OrderedBy _       -> "ordered-by"
            | Organization _    -> "organization"
            | Path _            -> "path"
            | Pattern _         -> "pattern"
            | Position _        -> "position"
            | Prefix _          -> "prefix"
            | Presence _        -> "presence"
            | Range _           -> "range"
            | Reference _       -> "reference"
            | Refine _          -> "refine"
            | RequireInstance _ -> "require-instance"
            | Revision _        -> "revision"
            | RevisionDate _    -> "revision-date"
            | Status _          -> "status"
            | Type _            -> "type"
            | TypeDef _         -> "typedef"
            | Unique _          -> "unique"
            | Units _           -> "units"
            | Value _           -> "value"
            | When _            -> "when"
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
            | Contact       (s, b)
            | Description   (s, b)
            | ErrorAppTag   (s, b)
            | ErrorMessage  (s, b)
            | Organization  (s, b)
            | Prefix        (s, b)
            | Presence      (s, b)
            | Reference     (s, b)
                -> sprintf "%s %s %s" id (ps s) (pb b)

            | Namespace (uri, block)        -> sprintf "%s %s %s" id (uri.ToString()) (pb block)
            | YangVersion (version, block)  -> sprintf "%s %s %s" id (version.ToString()) (pb block)

            | Unknown (id, arg, body)       -> sprintf "Unknown: %A %s %s"  id (pso arg) (pb body)
            | Unparsed (id, arg, body)      -> sprintf "Unparsed: %A %s %s" id (pso arg) (pb body)

    /// Short name for the extra statements that may appear
    and ExtraStatements         = Statement list option

    and AnyDataBodyStatement    =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Must          of MustStatement
    | Config        of ConfigStatement
    | Mandatory     of MandatoryStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    /// Captures the 'anydata-stmt' statement from [RFC 7950, p. 197]
    and AnyDataStatement        = Identifier            * (AnyDataBodyStatement list option)
    and AnyXmlBodyStatement     =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Must          of MustStatement
    | Config        of ConfigStatement
    | Mandatory     of MandatoryStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    /// Captures the 'anyxml-stmt' statement from [RFC 7950, p. 197]
    and AnyXmlStatement         = Identifier            * (AnyXmlBodyStatement list option)
    and ArgumentBodyStatement   =
    | YinElement    of YinElementStatement
    | Unknown       of UnknownStatement
    and ArgumentStatement       = Identifier            * (ArgumentBodyStatement list option)
    and BaseStatement           = IdentifierReference   * ExtraStatements
    and BelongsToBodyStatement  =
    | Prefix        of PrefixStatement
    | Unknown       of UnknownStatement
    and BelongsToStatement      = Identifier    * (BelongsToBodyStatement list)
    and BitBodyStatement        =
    | IfFeature     of IfFeatureStatement
    | Position      of PositionStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and BitStatement            = Identifier    * (BitBodyStatement list option)
    and ConfigStatement         = bool          * ExtraStatements
    and ContactStatement        = string        * ExtraStatements
    and DefaultStatement        = string        * ExtraStatements
    and DescriptionStatement    = string        * ExtraStatements
    and DeviateAddBodyStatement =
    | Units         of UnitsStatement
    | Must          of MustStatement
    | Unique        of UniqueStatement
    | Default       of DefaultStatement
    | Config        of ConfigStatement
    | Mandatory     of MandatoryStatement
    | MinElements   of MinElementsStatement
    | MaxElements   of MaxElementsStatement
    | Unknown       of UnknownStatement
    /// Captures the 'deviate-add-stmt' statement from [RFC 7950, p. 201].
    and DeviateAddStatement             = DeviateAddBodyStatement list option
    /// Captures the 'deviate-not-supported-stmt' statement from [RFC 7950, p. 201]; this statement does not take proper arguments.
    and DeviateDeleteBobyStatement      =
    | Units         of UnitsStatement
    | Must          of MustStatement
    | Unique        of UniqueStatement
    | Default       of DefaultStatement
    | Unknown       of UnknownStatement
    /// Captures the 'deviate-delete-stmt' statement from [RFC 7950, p. 201].
    and DeviateDeleteStatement          = DeviateDeleteBobyStatement list option
    and DeviateReplaceBodyStatement     =
    | Type          of TypeStatement
    | Units         of UnitsStatement
    | Default       of DefaultStatement
    | Config        of ConfigStatement
    | Mandatory     of MandatoryStatement
    | MinElements   of MinElementsStatement
    | MaxElements   of MaxElementsStatement
    | Unknown               of UnknownStatement
    and DeviateReplaceStatement         = DeviateReplaceBodyStatement list option
    and DeviateNotSupportedStatement    = ExtraStatements
    and DeviationBodyStatement  =
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | DeviateNotSupported   of DeviateNotSupportedStatement
    | DeviateAdd            of DeviateAddStatement
    | DeviateReplace        of DeviateReplaceStatement
    | DeviateDelete         of DeviateDeleteStatement
    | Unknown               of UnknownStatement
    and DeviationStatement      = Deviation     * (DeviationBodyStatement list option)
    and EnumBodyStatement       =
    | IfFeature     of IfFeatureStatement
    | Value         of ValueStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and EnumStatement           = string        * (EnumBodyStatement list option)
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
    and FractionDigitsStatement = byte                  * ExtraStatements
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
    and LeafBodyStatement       =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Type          of TypeStatement
    | Units         of UnitsStatement
    | Must          of MustStatement
    | Default       of DefaultStatement
    | Config        of ConfigStatement
    | Mandatory     of MandatoryStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and LeafStatement           = Identifier    * (LeafBodyStatement list)
    and LeafListBodyStatement   =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Type          of TypeStatement
    | Units         of UnitsStatement
    | Must          of MustStatement
    | Default       of DefaultStatement
    | Config        of ConfigStatement
    | MinElements   of MinElementsStatement
    | MaxElements   of MaxElementsStatement
    | OrderedBy     of OrderedByStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and LeafListStatement       = Identifier    * (LeafListBodyStatement list)
    and LengthBodyStatement     =
    | ErrorMessage  of ErrorMessageStatement
    | ErrorAppTag   of ErrorAppTagStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and LengthStatement         = Length        * (LengthBodyStatement list option)
    and MandatoryStatement      = bool          * ExtraStatements
    and MaxElementsStatement    = MaxValue      * ExtraStatements
    and MinElementsStatement    = uint32        * ExtraStatements
    and ModifierStatement       = Modifier      * ExtraStatements
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
    and PathStatement           = (Path list)   * ExtraStatements
    and PatternBodyStatement    =
    | Modifier      of ModifierStatement
    | ErrorMessage  of ErrorMessageStatement
    | ErrorAppTag   of ErrorAppTagStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and PatternStatement        = string        * (PatternBodyStatement list option)
    and PositionStatement       = uint32        * ExtraStatements
    and PrefixStatement         = string        * ExtraStatements
    and PresenceStatement       = string        * ExtraStatements
    and RangeBodyStatement      =
    | ErrorMessage  of ErrorMessageStatement
    | ErrorAppTag   of ErrorAppTagStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    /// Captures the 'range-stmt' statement from [RFC 7950, p. 189]
    and RangeStatement          = Range     * (RangeBodyStatement list option)
    and ReferenceStatement      = string    * ExtraStatements
    and RefineBodyStatement     =
    | IfFeature     of IfFeatureStatement
    | Must          of MustStatement
    | Presence      of PresenceStatement
    | Default       of DefaultStatement
    | Config        of ConfigStatement
    | Mandatory     of MandatoryStatement
    | MinElements   of MinElementsStatement
    | MaxElements   of MaxElementsStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    /// Captures the 'refine-stmt' statement from [RFC 7950, p. 198]
    and RefineStatement         = Refine    * (RefineBodyStatement list option)
    and RevisionBodyStatement   =
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and RevisionStatement       = Arguments.Date    * (RevisionBodyStatement list option)
    and RevisionDateStatement   = Arguments.Date    * ExtraStatements
    and RequireInstanceStatement = bool             * ExtraStatements
    and StatusStatement         = Status            * ExtraStatements
    and TypeBodyStatement       =
    | NumericalRestrictions             of NumericalRestrictions
    | Decimal64Specification            of Decimal64Specification
    | StringRestrictions                of StringRestrictions
    | EnumSpecification                 of EnumSpecification
    | LeafRefSpecification              of LeafRefSpecification
    | IdentityRefSpecification          of IdentityRefSpecification
    | InstanceIdentifierSpecification   of InstanceIdentifierSpecification
    | BitsSpecification                 of BitsSpecification
    | UnionSpecification                of UnionSpecification
    | BinarySpecification               of BinarySpecification
    and TypeDefBodyStatement    =
    | Type          of TypeStatement
    | Units         of UnitsStatement
    | Default       of DefaultStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and TypeDefStatement        = Identifier        * (TypeDefBodyStatement list option)
    /// Captures a type-stmt [RFC 7950, p.188]. If there are unknown statements, then they precede the TypeBodyStatement
    and TypeStatement           = IdentifierReference   * (TypeBodyStatement option) * (UnknownStatement list option)
    and UniqueStatement         = Unique            * ExtraStatements
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

    and BinarySpecification             = LengthStatement option
    and BitsSpecification               = BitStatement list
    and Decimal64Specification          = FractionDigitsStatement   * (RangeStatement option)
    and EnumSpecification               = EnumStatement list
    and IdentityRefSpecification        = BaseStatement list
    and InstanceIdentifierSpecification = RequireInstanceStatement option
    and LeafRefSpecification            = PathStatement             * (RequireInstanceStatement option)
    and NumericalRestrictions           = RangeStatement
    and StringRestrictions              = (LengthStatement option)  * (PatternStatement list)
    and UnionSpecification              = TypeStatement list