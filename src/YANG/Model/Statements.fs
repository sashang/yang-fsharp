// Statenents.fs
namespace Yang.Model

/// Definition of Yang statements
[<AutoOpen>]
module Statements =
    open System
    open Arguments
    open System.ComponentModel
    open System.Runtime.Remoting.Messaging
    open System.Reflection

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
     *
     * In some cases, the YANG model specifies mutually exclusive alternatives. These are not captured in
     * the model below, instead we allow both alternatives. The caller needs to apply extra check to guarantee
     * that the YANG model requirements are met.
     *)

    /// Available Yang statement definitions
    [<StructuredFormatDisplay("{PrettyPrint}")>]
    type Statement =
    | Action        of ActionStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Argument      of ArgumentStatement
    | Augment       of AugmentStatement
    | Base          of BaseStatement
    | BelongsTo     of BelongsToStatement
    | Bit           of BitStatement
    | Case          of CaseStatement
    | Choice        of ChoiceStatement
    | Config        of ConfigStatement
    | Contact       of ContactStatement
    | Container     of ContainerStatement
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
    | Grouping      of GroupingStatement
    | Identity      of IdentityStatement
    | IfFeature     of IfFeatureStatement
    | Import        of ImportStatement
    | Include       of IncludeStatement
    | Input         of InputStatement
    | Key           of KeyStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | Length        of LengthStatement
    | List          of ListStatement
    | Mandatory     of MandatoryStatement
    | MaxElements   of MaxElementsStatement
    | MinElements   of MinElementsStatement
    | Modifier      of ModifierStatement
    | Module        of ModuleStatement
    | Must          of MustStatement
    | Namespace     of NamespaceStatement
    | Notification  of NotificationStatement
    | OrderedBy     of OrderedByStatement
    | Organization  of OrganizationStatement
    | Output        of OutputStatement
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
    | Rpc           of RpcStatement
    | RequireInstance of RequireInstanceStatement
    | Status        of StatusStatement
    | Submodule     of SubmoduleStatement
    | Type          of TypeStatement
    | TypeDef       of TypeDefStatement
    | Unique        of UniqueStatement
    | Units         of UnitsStatement
    | Uses          of UsesStatement
    | Value         of ValueStatement
    | When          of WhenStatement
    | YangVersion   of YangVersionStatement
    | YinElement    of YinElementStatement
    | Unknown       of UnknownStatement
    | Unparsed      of Identifier:Identifier * Argument:(string option) * Body:(Statement list option)
    | UsesAugment   of UsesAugmentStatement
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
            | Action        (_, _)
            | AnyData       (_, _)
            | AnyXml        (_, _)
            | Augment       (_, _)
            | Argument      (_, _)
            | BelongsTo     (_, _)
            | Bit           (_, _)
            | Case          (_, _)
            | Choice        (_, _)
            | Container     (_, _)
            | DeviateAdd     _
            | DeviateDelete  _
            | DeviateReplace _
            | Deviation     (_, _)
            | Enum          (_, _)
            | Extension     (_, _)
            | Feature       (_, _)
            | Grouping      (_, _)
            | Identity      (_, _)
            | Import        (_, _)
            | Include       (_, _)
            | Input         _
            | Leaf          (_, _)
            | LeafList      (_, _)
            | Length        (_, _)
            | List          (_, _)
            | Module        _
            | Must          (_, _)
            | Notification  (_, _)
            | Output        _
            | Pattern       (_, _)
            | Range         (_, _)
            | Rpc           (_, _)
            | Refine        (_, _)
            | Revision      (_, _)
            | Submodule     _
            | Type          (_, _, _)
            | TypeDef       (_, _)
            | Uses          (_, _)
            | UsesAugment   (_, _)
            | When          (_, _)
                -> None

            | Unknown _
            | Unparsed _
                -> None

        member this.Identifier =
            match this with
            | Action _          -> "action"
            | AnyData _         -> "anydata"
            | AnyXml _          -> "anyxml"
            | Argument _        -> "argument"
            | Augment _         -> "augment"
            | Base _            -> "base"
            | BelongsTo _       -> "belongs-to"
            | Bit _             -> "bit"
            | Case _            -> "case"
            | Choice _          -> "choice"
            | Config _          -> "config"
            | Contact _         -> "contact"
            | Container _       -> "container"
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
            | Grouping _        -> "grouping"
            | Identity _        -> "identity"
            | IfFeature _       -> "if-feature"
            | Import _          -> "import"
            | Include _         -> "include"
            | Input _           -> "input"
            | Key _             -> "key"
            | Leaf _            -> "leaf"
            | LeafList _        -> "leaf-list"
            | Length _          -> "length"
            | List _            -> "list"
            | Mandatory _       -> "mandatory"
            | MaxElements _     -> "max-elements"
            | MinElements _     -> "min-elements"
            | Modifier _        -> "modifier"
            | Module _          -> "module"
            | Must _            -> "must"
            | Namespace _       -> "namespace"
            | Notification _    -> "notification"
            | OrderedBy _       -> "ordered-by"
            | Organization _    -> "organization"
            | Output _          -> "output"
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
            | Rpc _             -> "rpc"
            | Status _          -> "status"
            | Submodule _       -> "submodule"
            | Type _            -> "type"
            | TypeDef _         -> "typedef"
            | Unique _          -> "unique"
            | Units _           -> "units"
            | Uses _            -> "uses"
            // The following uses the same keyword as Uses.
            | UsesAugment _     -> "uses"
            | Value _           -> "value"
            | When _            -> "when"
            | YangVersion _     -> "yang-version"
            | YinElement _      -> "yin-element"
            | Unknown (id, _, _)    -> id.ToString()
            | Unparsed (id, _, _)   -> id.ToString()

        member this.PrettyPrint = Printer.Print this

    /// Short name for the extra statements that may appear
    and ExtraStatements         = Statement list option

    and ActionBodyStatement     =
    | IfFeature     of IfFeatureStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | TypeDef       of TypeDefStatement
    | Grouping      of GroupingStatement
    | Input         of InputStatement
    | Output        of OutputStatement
    | Unknown       of UnknownStatement
    /// Captures the 'action-stmt' statement from [RFC 7950, p. 200]
    and ActionStatement         = Identifier            * (ActionBodyStatement list option)
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
    and AugmentBodyStatement    =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Case          of CaseStatement
    | Action        of ActionStatement
    | Notification  of NotificationStatement
    | Unknown       of UnknownStatement
    /// Captures the 'augment-stmt' statement from [RFC 7950, p. 199]
    and AugmentStatement        = Augment               * (AugmentBodyStatement list)
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
    and CaseBodyStatement       =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Unknown       of UnknownStatement
    /// Captures the 'case-stmt' statement from [RFC 7950, p. 196].
    and CaseStatement           = Identifier    * (CaseBodyStatement list option)
    and ChoiceBodyStatement     =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Default       of DefaultStatement
    | Config        of ConfigStatement
    | Mandatory     of MandatoryStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    // short-case-stmt
    | Choice        of ChoiceStatement
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    // end of short-case-stmt
    | Case          of CaseStatement
    | Unknown       of UnknownStatement
    /// Captures the 'choice-stmt' statement from [RFC 7950, p. 196].
    and ChoiceStatement         = Identifier    * (ChoiceBodyStatement list option)
    and ConfigStatement         = bool          * ExtraStatements
    and ContactStatement        = string        * ExtraStatements
    and ContainerBodyStatement  =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Must          of MustStatement
    | Presence      of PresenceStatement
    | Config        of ConfigStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | TypeDef       of TypeDefStatement
    | Grouping      of GroupingStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Action        of ActionStatement
    | Notification  of NotificationStatement
    | Unknown       of UnknownStatement
    /// Captures the 'container-stmt' statement from [RFC 7950, p. 193].
    and ContainerStatement      = Identifier    * (ContainerBodyStatement list option)
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
    | Unknown       of UnknownStatement
    and FeatureStatement        = Identifier            * (FeatureBodyStatement list option)
    and FractionDigitsStatement = byte                  * ExtraStatements
    and GroupingBodyStatement =
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | TypeDef       of TypeDefStatement
    | Grouping      of GroupingStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Action        of ActionStatement
    | Notification  of NotificationStatement
    | Unknown       of UnknownStatement
    /// Captures the 'grouping-stmt' statement from [RFC 7950, p. 193]
    and GroupingStatement       = Identifier            * (GroupingBodyStatement list option)
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
    and InputBodyStatement      =
    | Must          of MustStatement
    | TypeDef       of TypeDefStatement
    | Grouping      of GroupingStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Unknown       of UnknownStatement
    /// Captures the 'input-stmt' statement from [RFC 7950, p. 200]
    and InputStatement          = InputBodyStatement list
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
    and ListBodyStatement       =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Must          of MustStatement
    | Key           of KeyStatement
    | Unique        of UniqueStatement
    | Config        of ConfigStatement
    | MinElements   of MinElementsStatement
    | MaxElements   of MaxElementsStatement
    | OrderedBy     of OrderedByStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | TypeDef       of TypeDefStatement
    | Grouping      of GroupingStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Action        of ActionStatement
    | Notification  of NotificationStatement
    | Unknown       of UnknownStatement
    /// Captures the 'list-stmt' statement from [RFC 7950, p. 195]
    and ListStatement           = Identifier    * (ListBodyStatement list)
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
    and NotificationBodyStatement =
    | IfFeature     of IfFeatureStatement
    | Must          of MustStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | TypeDef       of TypeDefStatement
    | Grouping      of GroupingStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Unknown       of UnknownStatement
    /// Captures the 'notification-stmt' statement from [RFC 7950, p. 200]
    and NotificationStatement   = Identifier    * (NotificationBodyStatement list option)
    and OrderedByStatement      = OrderedBy     * ExtraStatements
    and OrganizationStatement   = string        * ExtraStatements
    and OutputBodyStatement     =
    | Must          of MustStatement
    | TypeDef       of TypeDefStatement
    | Grouping      of GroupingStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Unknown       of UnknownStatement
    /// Captures the 'output-stmt' statement from [RFC 7950, p. 200]
    and OutputStatement         = OutputBodyStatement list
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
    and RpcBodyStatement        =
    | IfFeature     of IfFeatureStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | TypeDef       of TypeDefStatement
    | Grouping      of GroupingStatement
    | Input         of InputStatement
    | Output        of OutputStatement
    | Unknown       of UnknownStatement
    /// Captures the 'rpc-stmt' statement from [RFC 7950, p. 199]
    and RpcStatement            = Identifier        * (RpcBodyStatement list option)
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
    /// Captures the type-stmt [RFC 7950, p.188]. If there are unknown statements, then they precede the TypeBodyStatement
    and TypeStatement           = IdentifierReference   * (TypeBodyStatement option) * (UnknownStatement list option)
    and UniqueStatement         = Unique            * ExtraStatements
    and UnitsStatement          = string            * ExtraStatements
    and UsesBodyStatement       =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Refine        of RefineStatement
    | UsesAugment   of UsesAugmentStatement
    | Unknown       of UnknownStatement
    /// Captures the uses-stmt [RFC 7950, p.197].
    and UsesStatement           = IdentifierReference   * (UsesBodyStatement list option)
    and UsesAugmentBodyStatement =
    | When          of WhenStatement
    | IfFeature     of IfFeatureStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Case          of CaseStatement
    | Action        of ActionStatement
    | Notification  of NotificationStatement
    | Unknown       of UnknownStatement
    /// Captures the uses-augment-stmt [RFC 7950, p.198].
    and UsesAugmentStatement    = UsesAugment       * (UsesAugmentBodyStatement list)
    and ValueStatement          = int64             * ExtraStatements
    and WhenBodyStatement       =
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and WhenStatement           = string            * (WhenBodyStatement list option)
    and YangVersionStatement    = Version           * ExtraStatements
    and YinElementStatement     = bool              * ExtraStatements

    (* DataDef should be replaced by the following:
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    *)

    (* Body should be replaced by the following:
    // body-stmts
    | Extension    of ExtensionStatement
    | Feature      of FeatureStatement
    | Identity     of IdentityStatement
    | TypeDef      of TypeDefStatement
    | Grouping     of GroupingStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Augment       of AugmentStatement
    | Rpc           of RpcStatement
    | Notification  of NotificationStatement
    | Deviation     of DeviationStatement
    // end of body-stmts
     *)

    /// This captures all user defined statements; [RFC 7950, p. 202]
    and UnknownStatement        = IdentifierWithPrefix * (string option) * ExtraStatements

    // The following types are used in the definition of the module and sub-module statements

    and ModuleHeaderStatements      = YangVersionStatement * NamespaceStatement * PrefixStatement
    and SubmoduleHeaderStatements   = YangVersionStatement * BelongsToStatement
    and LinkageBodyStatement        =
    | Import        of ImportStatement
    | Include       of IncludeStatement
    and LinkageStatements           = LinkageBodyStatement list
    and MetaBodyStatement           =
    | Organization  of OrganizationStatement
    | Contact       of ContactStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    and MetaStatements              = MetaBodyStatement list
    and BodyStatement               =
    // body-stmts
    | Extension    of ExtensionStatement
    | Feature      of FeatureStatement
    | Identity     of IdentityStatement
    | TypeDef      of TypeDefStatement
    | Grouping     of GroupingStatement
    // data-def-stmt
    | Container     of ContainerStatement
    | Leaf          of LeafStatement
    | LeafList      of LeafListStatement
    | List          of ListStatement
    | Choice        of ChoiceStatement
    | AnyData       of AnyDataStatement
    | AnyXml        of AnyXmlStatement
    | Uses          of UsesStatement
    // End of data-def-stmt
    | Augment       of AugmentStatement
    | Rpc           of RpcStatement
    | Notification  of NotificationStatement
    | Deviation     of DeviationStatement
    // end of body-stmts
    | Unknown       of UnknownStatement

    and ModuleStatement             = {
        Name        : Identifier
        Header      : ModuleHeaderStatements
        Linkage     : LinkageStatements
        Meta        : MetaStatements
        Revision    : RevisionStatement list
        Body        : BodyStatement list
    }
    and SubmoduleStatement          = {
        Name        : Identifier
        Header      : SubmoduleHeaderStatements
        Linkage     : LinkageStatements
        Meta        : MetaStatements
        Revision    : RevisionStatement list
        Body        : BodyStatement list
    }
    and Printer ()                  =
        class
            static let mutable StatementPrinter : (Statement -> string) option = None
            static member Set (printer : Statement -> string) = StatementPrinter <- Some printer
            static member Print (st : Statement) =
                match StatementPrinter with
                | None          -> sprintf "%A" st
                | Some printer  -> printer st
        end

    // Helper types that are not exported as statements

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

    (*
     * End of Statement and related definitions
     *)

    /// Helper methods for the ActionBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ActionBodyStatement =
        let Translate = function
        | ActionBodyStatement.IfFeature     st -> Statement.IfFeature   st
        | ActionBodyStatement.Status        st -> Statement.Status      st
        | ActionBodyStatement.Description   st -> Statement.Description st
        | ActionBodyStatement.Reference     st -> Statement.Reference   st
        | ActionBodyStatement.TypeDef       st -> Statement.TypeDef     st
        | ActionBodyStatement.Grouping      st -> Statement.Grouping    st
        | ActionBodyStatement.Input         st -> Statement.Input       st
        | ActionBodyStatement.Output        st -> Statement.Output      st
        | ActionBodyStatement.Unknown       st -> Statement.Unknown     st

    /// Helper methods for the AnyDataBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnyDataBodyStatement =

        let Translate = function
        | AnyDataBodyStatement.When         st -> Statement.When        st
        | AnyDataBodyStatement.IfFeature    st -> Statement.IfFeature   st
        | AnyDataBodyStatement.Must         st -> Statement.Must        st
        | AnyDataBodyStatement.Config       st -> Statement.Config      st
        | AnyDataBodyStatement.Mandatory    st -> Statement.Mandatory   st
        | AnyDataBodyStatement.Status       st -> Statement.Status      st
        | AnyDataBodyStatement.Description  st -> Statement.Description st
        | AnyDataBodyStatement.Reference    st -> Statement.Reference   st
        | AnyDataBodyStatement.Unknown      st -> Statement.Unknown     st

    /// Helper methods for the AnyXmlBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnyXmlBodyStatement =

        let Translate = function
        | AnyXmlBodyStatement.When          st -> Statement.When        st
        | AnyXmlBodyStatement.IfFeature     st -> Statement.IfFeature   st
        | AnyXmlBodyStatement.Must          st -> Statement.Must        st
        | AnyXmlBodyStatement.Config        st -> Statement.Config      st
        | AnyXmlBodyStatement.Mandatory     st -> Statement.Mandatory   st
        | AnyXmlBodyStatement.Status        st -> Statement.Status      st
        | AnyXmlBodyStatement.Description   st -> Statement.Description st
        | AnyXmlBodyStatement.Reference     st -> Statement.Reference   st
        | AnyXmlBodyStatement.Unknown       st -> Statement.Unknown     st

    /// Helper methods for the ArgumentBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArgumentBodyStatement =

        let Translate = function
        | ArgumentBodyStatement.YinElement  st -> Statement.YinElement  st
        | ArgumentBodyStatement.Unknown     st -> Statement.Unknown     st

    /// Helper methods for the AugmentBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AugmentBodyStatement =

        let Translate = function
        | AugmentBodyStatement.When          st -> Statement.When           st
        | AugmentBodyStatement.IfFeature     st -> Statement.IfFeature      st
        | AugmentBodyStatement.Status        st -> Statement.Status         st
        | AugmentBodyStatement.Description   st -> Statement.Description    st
        | AugmentBodyStatement.Reference     st -> Statement.Reference      st
        | AugmentBodyStatement.Container     st -> Statement.Container      st
        | AugmentBodyStatement.Leaf          st -> Statement.Leaf           st
        | AugmentBodyStatement.LeafList      st -> Statement.LeafList       st
        | AugmentBodyStatement.List          st -> Statement.List           st
        | AugmentBodyStatement.Choice        st -> Statement.Choice         st
        | AugmentBodyStatement.AnyData       st -> Statement.AnyData        st
        | AugmentBodyStatement.AnyXml        st -> Statement.AnyXml         st
        | AugmentBodyStatement.Uses          st -> Statement.Uses           st
        | AugmentBodyStatement.Case          st -> Statement.Case           st
        | AugmentBodyStatement.Action        st -> Statement.Action         st
        | AugmentBodyStatement.Notification  st -> Statement.Notification   st
        | AugmentBodyStatement.Unknown       st -> Statement.Unknown        st

    /// Helper methods for the BelongsToBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BelongsToBodyStatement =

        let Translate = function
        | BelongsToBodyStatement.Prefix     st -> Statement.Prefix      st
        | BelongsToBodyStatement.Unknown    st -> Statement.Unknown     st

    /// Helper methods for the Statement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Statement =
        open System.Text

        /// Retrieves the extra options that may appear at the end of the statement;
        /// If the statement has a statement specific body, then this call will return None,
        /// and the caller will need to apply per-statement processing to retrieve those statements.
        let Options (this : Statement) =
            match this with
            | Statement.Base                (_, options)
            | Statement.Config              (_, options)
            | Statement.Contact             (_, options)
            | Statement.Default             (_, options)
            | Statement.Description         (_, options)
            | Statement.DeviateNotSupported options
            | Statement.ErrorAppTag         (_, options)
            | Statement.ErrorMessage        (_, options)
            | Statement.FractionDigits      (_, options)
            | Statement.IfFeature           (_, options)
            | Statement.Key                 (_, options)
            | Statement.Mandatory           (_, options)
            | Statement.MaxElements         (_, options)
            | Statement.MinElements         (_, options)
            | Statement.Modifier            (_, options)
            | Statement.Namespace           (_, options)
            | Statement.OrderedBy           (_, options)
            | Statement.Organization        (_, options)
            | Statement.Path                (_, options)
            | Statement.Position            (_, options)
            | Statement.Prefix              (_, options)
            | Statement.Presence            (_, options)
            | Statement.Reference           (_, options)
            | Statement.RevisionDate        (_, options)
            | Statement.RequireInstance     (_, options)
            | Statement.Status              (_, options)
            | Statement.Units               (_, options)
            | Statement.Unique              (_, options)
            | Statement.Value               (_, options)
            | Statement.YangVersion         (_, options)
            | Statement.YinElement          (_, options)
                -> options

            // The following have custom options; the caller need to treat them specially
            | Statement.Action              (_, _)
            | Statement.AnyData             (_, _)
            | Statement.AnyXml              (_, _)
            | Statement.Augment             (_, _)
            | Statement.Argument            (_, _)
            | Statement.BelongsTo           (_, _)
            | Statement.Bit                 (_, _)
            | Statement.Case                (_, _)
            | Statement.Choice              (_, _)
            | Statement.Container           (_, _)
            | Statement.DeviateAdd          _
            | Statement.DeviateDelete       _
            | Statement.DeviateReplace      _
            | Statement.Deviation           (_, _)
            | Statement.Enum                (_, _)
            | Statement.Extension           (_, _)
            | Statement.Feature             (_, _)
            | Statement.Grouping            (_, _)
            | Statement.Identity            (_, _)
            | Statement.Import              (_, _)
            | Statement.Include             (_, _)
            | Statement.Input               _
            | Statement.Leaf                (_, _)
            | Statement.LeafList            (_, _)
            | Statement.Length              (_, _)
            | Statement.List                (_, _)
            | Statement.Module              _
            | Statement.Must                (_, _)
            | Statement.Notification        (_, _)
            | Statement.Output              _
            | Statement.Pattern             (_, _)
            | Statement.Range               (_, _)
            | Statement.Rpc                 (_, _)
            | Statement.Refine              (_, _)
            | Statement.Revision            (_, _)
            | Statement.Submodule           _
            | Statement.Type                (_, _, _)
            | Statement.TypeDef             (_, _)
            | Statement.Uses                (_, _)
            | Statement.UsesAugment         (_, _)
            | Statement.When                (_, _)
                -> None

            | Statement.Unknown _
            | Statement.Unparsed _
                -> None

        let Keyword (this : Statement) =
            match this with
            | Statement.Action _                -> "action"
            | Statement.AnyData _               -> "anydata"
            | Statement.AnyXml _                -> "anyxml"
            | Statement.Argument _              -> "argument"
            | Statement.Augment _               -> "augment"
            | Statement.Base _                  -> "base"
            | Statement.BelongsTo _             -> "belongs-to"
            | Statement.Bit _                   -> "bit"
            | Statement.Case _                  -> "case"
            | Statement.Choice _                -> "choice"
            | Statement.Config _                -> "config"
            | Statement.Contact _               -> "contact"
            | Statement.Container _             -> "container"
            | Statement.Default _               -> "default"
            | Statement.Description _           -> "description"
            | Statement.DeviateAdd _            -> "deviate add"
            | Statement.DeviateDelete _         -> "deviate delete"
            | Statement.DeviateNotSupported _   -> "deviate not-supported"
            | Statement.DeviateReplace _        -> "deviate replace"
            | Statement.Deviation _             -> "deviation"
            | Statement.Enum _                  -> "enum"
            | Statement.ErrorAppTag _           -> "error-app-tag"
            | Statement.ErrorMessage _          -> "error-message"
            | Statement.Extension _             -> "extension"
            | Statement.Feature _               -> "feature"
            | Statement.FractionDigits _        -> "fraction-digits"
            | Statement.Grouping _              -> "grouping"
            | Statement.Identity _              -> "identity"
            | Statement.IfFeature _             -> "if-feature"
            | Statement.Import _                -> "import"
            | Statement.Include _               -> "include"
            | Statement.Input _                 -> "input"
            | Statement.Key _                   -> "key"
            | Statement.Leaf _                  -> "leaf"
            | Statement.LeafList _              -> "leaf-list"
            | Statement.Length _                -> "length"
            | Statement.List _                  -> "list"
            | Statement.Mandatory _             -> "mandatory"
            | Statement.MaxElements _           -> "max-elements"
            | Statement.MinElements _           -> "min-elements"
            | Statement.Modifier _              -> "modifier"
            | Statement.Module _                -> "module"
            | Statement.Must _                  -> "must"
            | Statement.Namespace _             -> "namespace"
            | Statement.Notification _          -> "notification"
            | Statement.OrderedBy _             -> "ordered-by"
            | Statement.Organization _          -> "organization"
            | Statement.Output _                -> "output"
            | Statement.Path _                  -> "path"
            | Statement.Pattern _               -> "pattern"
            | Statement.Position _              -> "position"
            | Statement.Prefix _                -> "prefix"
            | Statement.Presence _              -> "presence"
            | Statement.Range _                 -> "range"
            | Statement.Reference _             -> "reference"
            | Statement.Refine _                -> "refine"
            | Statement.RequireInstance _       -> "require-instance"
            | Statement.Revision _              -> "revision"
            | Statement.RevisionDate _          -> "revision-date"
            | Statement.Rpc _                   -> "rpc"
            | Statement.Status _                -> "status"
            | Statement.Submodule _             -> "submodule"
            | Statement.Type _                  -> "type"
            | Statement.TypeDef _               -> "typedef"
            | Statement.Unique _                -> "unique"
            | Statement.Units _                 -> "units"
            | Statement.Uses _                  -> "uses"
            // The following uses the same keyword as Uses.
            | Statement.UsesAugment _           -> "uses"
            | Statement.Value _                 -> "value"
            | Statement.When _                  -> "when"
            | Statement.YangVersion _           -> "yang-version"
            | Statement.YinElement _            -> "yin-element"
            | Statement.Unknown (id, _, _)      -> id.ToString()
            | Statement.Unparsed (id, _, _)     -> id.ToString()

        let rec internal print (sb : StringBuilder, tabs : int) (this : Statement) =
            let escape = [| ' '; '\t'; '\r'; '\n'; ';'; '{'; '}'; '@'; ':' |]

            /// Print string
            let ps (input : string) =
                if input.IndexOfAny(escape) > 0 then Printf.bprintf sb "\"%s\"" input
                else Printf.bprintf sb "%s" input

            /// Print string and end line
            let pse (input : string) = ps input; Printf.bprintf sb ";"

            /// Print identifier
            let psi (id : Identifier) = ps (id.ToString())

            /// Print space
            let sp () = ps " "

            /// Print new line
            let nl () = Printf.bprintf sb "\n%s" (String.replicate tabs "\t")

            /// Print string in new line
            let psnl (input : string) =
                Printf.bprintf sb "\n"
                Printf.bprintf sb "%s%s" (String.replicate tabs "\t") input

            /// Print optional string
            let pso (input : string option) =
                match input with
                | Some str  -> ps str
                | None      -> ps ""

            /// Print generic argument
            let psa (argument : 'a) = Printf. bprintf sb "%A" argument

            /// Print end of statement (';') or block
            let pb (options : ExtraStatements) =
                match options with
                | None          -> pse ""
                | Some block    ->
                    ps " {"
                    block |> List.iter (fun s -> print (sb, tabs+1) s; nl ())
                    psnl "}\n"

            /// Print optional block of statements
            let pbo (translate : 'T -> Statement) (block : 'T list option) =
                match block with
                | None -> pse ""
                | Some block ->
                    if block.Length = 0 then pse " {}"
                    else
                        pse " {"
                        block |> List.map translate |> List.iter (fun st -> print (sb, tabs + 1) st; nl ())
                        pse "}"

            /// Print generic block of statement
            let pbt (translate : 'T -> Statement) (block : 'T list) =
                pse " {"
                block |> List.map translate |> List.iter (fun st -> print (sb, tabs + 1) st; nl ())
                pse "}"

            let keyword = Keyword this
            ps keyword; sp ()

            match this with
            //| Statement.Config              (s, block)
            | Statement.Contact             (s, block)
            //| Statement.Default             (s, block)
            | Statement.Description         (s, block)
            | Statement.ErrorAppTag         (s, block)
            | Statement.ErrorMessage        (s, block)
            //| Statement.FractionDigits      (s, block)
            //| Statement.IfFeature           (s, block)
            //| Statement.Key                 (s, block)
            //| Statement.Mandatory           (s, block)
            //| Statement.MaxElements         (s, block)
            //| Statement.MinElements         (s, block)
            //| Statement.Modifier            (s, block)
            //| Statement.OrderedBy           (s, block)
            | Statement.Organization        (s, block)
            //| Statement.Path                (s, block)
            //| Statement.Position            (s, block)
            | Statement.Prefix              (s, block)
            //| Statement.Presence            (s, block)
            | Statement.Reference           (s, block)
            //| Statement.RevisionDate        (s, block)
            //| Statement.RequireInstance     (s, block)
            | Statement.Units               (s, block)
            //| Statement.Unique              (s, block)
            //| Statement.Value               (s, block)
            //| Statement.YinElement          (s, block)
                                                                        -> ps s; pb block

            | Statement.Action                              (id, block) -> psi id; pbo ActionBodyStatement.Translate    block
            | Statement.AnyData                             (id, block) -> psi id; pbo AnyDataBodyStatement.Translate   block
            | Statement.AnyXml                              (id, block) -> psi id; pbo AnyXmlBodyStatement.Translate    block
            | Statement.Argument                            (id, block) -> psi id; pbo ArgumentBodyStatement.Translate  block
            | Statement.Augment                             (au, block) -> psa au; pbt AugmentBodyStatement.Translate block
            | Statement.BelongsTo                           (bt, block) -> psa bt; pbt BelongsToBodyStatement.Translate block

            | Statement.Status                          (status, block) -> psa status;  pb block
            | Statement.Namespace                          (uri, block) -> psa uri;     pb block
            | Statement.YangVersion                    (version, block) -> psa version; pb block

            | Statement.Base                                (_, block)
            | Statement.DeviateNotSupported                     block   -> pb block

            | Statement.Unknown                         (_, arg, body)
            | Statement.Unparsed                        (_, arg, body)  -> pso arg; pb body

        /// <summary>
        /// Get a string version of the statement (in YANG format)
        /// </summary>
        /// <param name="statement">The statement to print</param>
        let ToString statement = let sb = StringBuilder () in print (sb, 0) statement ; sb.ToString()

    do
        Printer.Set Statement.ToString
