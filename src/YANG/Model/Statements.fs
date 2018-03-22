// Statements.fs
// Provides definitions that track the statements defined in the YANG model.
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
     *
     * In some cases, the YANG model specifies mutually exclusive alternatives. These are not captured in
     * the model below, instead we allow both alternatives. The caller needs to apply extra check to guarantee
     * that the YANG model requirements are met.
     *)

     // TODO: Remove/fix pretty printing functionality from this file; use other version.

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
    | UsesAugment   of UsesAugmentStatement
    | Value         of ValueStatement
    | When          of WhenStatement
    | YangVersion   of YangVersionStatement
    | YinElement    of YinElementStatement
    | Unknown       of UnknownStatement
    with
        member this.PrettyPrint     = StatementPrinter.Print this
        override this.ToString()    = this.PrettyPrint

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
    /// Captures the 'argument-stmt' statement from [RFC 7950, p. 187]
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
    /// Captures the 'base-stmt' statement from [RFC 7950, p. 187]
    and BaseStatement           = IdentifierReference   * ExtraStatements
    and BelongsToBodyStatement  =
    | Prefix        of PrefixStatement
    | Unknown       of UnknownStatement
    /// Captures the 'belongs-to-stmt' statement from [RFC 7950, p. 186]
    and BelongsToStatement      = Identifier    * (BelongsToBodyStatement list)
    and BitBodyStatement        =
    | IfFeature     of IfFeatureStatement
    | Position      of PositionStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    /// Captures the 'bit-stmt' statement from [RFC 7950, p. 191]
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
    /// Captures the 'config-stmt' statement from [RFC 7950, p. 191]
    and ConfigStatement         = bool          * ExtraStatements
    /// Captures the 'contact-stmt' statement from [RFC 7950, p. 186]
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
    /// Captures the 'default-stmt' statement from [RFC 7950, p. 190]
    and DefaultStatement        = string        * ExtraStatements
    /// Captures the 'description-stmt' statement from [RFC 7950, p. 186]
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
    and DeviateDeleteBodyStatement      =
    | Units         of UnitsStatement
    | Must          of MustStatement
    | Unique        of UniqueStatement
    | Default       of DefaultStatement
    | Unknown       of UnknownStatement
    /// Captures the 'deviate-delete-stmt' statement from [RFC 7950, p. 201].
    and DeviateDeleteStatement          = DeviateDeleteBodyStatement list option
    and DeviateReplaceBodyStatement     =
    | Type          of TypeStatement
    | Units         of UnitsStatement
    | Default       of DefaultStatement
    | Config        of ConfigStatement
    | Mandatory     of MandatoryStatement
    | MinElements   of MinElementsStatement
    | MaxElements   of MaxElementsStatement
    | Unknown       of UnknownStatement
    /// Captures the 'deviate-replace-stmt' statement from [RFC 7950, p. 202].
    and DeviateReplaceStatement         = DeviateReplaceBodyStatement list option
    /// Captures the 'deviate-not-supported-stmt' statement from [RFC 7950, p. 201].
    and DeviateNotSupportedStatement    = ExtraStatements
    and DeviationBodyStatement  =
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | DeviateNotSupported   of DeviateNotSupportedStatement
    | DeviateAdd            of DeviateAddStatement
    | DeviateReplace        of DeviateReplaceStatement
    | DeviateDelete         of DeviateDeleteStatement
    | Unknown               of UnknownStatement
    /// Captures the 'deviation-stmt' statement from [RFC 7950, p. 201].
    and DeviationStatement      = Deviation     * (DeviationBodyStatement list option)
    and EnumBodyStatement       =
    | IfFeature     of IfFeatureStatement
    | Value         of ValueStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    /// Captures the 'enum-stmt' statement from [RFC 7950, p. 190].
    and EnumStatement           = string        * (EnumBodyStatement list option)
    /// Captures the 'error-app-tag-stmt' statement from [RFC 7950, p. 192].
    and ErrorAppTagStatement    = string        * ExtraStatements
    /// Captures the 'error-message-stmt' statement from [RFC 7950, p. 192].
    and ErrorMessageStatement   = string        * ExtraStatements
    and ExtensionBodyStatement  =
    | Argument      of ArgumentStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    /// Captures the 'extension-stmt' statement from [RFC 7950, p. 192].
    and ExtensionStatement      = Identifier            * (ExtensionBodyStatement list option)
    and FeatureBodyStatement    =
    | IfFeature     of IfFeatureStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    /// Captures the 'feature-stmt' statement from [RFC 7950, p. 187].
    and FeatureStatement        = Identifier            * (FeatureBodyStatement list option)
    /// Captures the 'fraction-digits-stmt' statement from [RFC 7950, p. 189].
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
    and IfFeatureStatement      = Expression            * ExtraStatements
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
    and MinElementsStatement    = MinValue      * ExtraStatements
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
    and PathStatement           = Path          * ExtraStatements
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
    /// Captures the 'type-stmt' statement from [RFC 7950, p. 188].
    /// The definition assumes a number of unknown statements (from the stmtsep),
    /// followed by zero or one type-body-stmts.
    and TypeStatement           = IdentifierReference   * (TypeBodyStatement option) * (UnknownStatement list option)
    and TypeDefBodyStatement    =
    | Type          of TypeStatement
    | Units         of UnitsStatement
    | Default       of DefaultStatement
    | Status        of StatusStatement
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement
    /// Captures the 'typedef-stmt' statement from [RFC 7950, p. 188].
    and TypeDefStatement        = Identifier        * (TypeDefBodyStatement list)
    /// Captures the type-stmt [RFC 7950, p.188]. If there are unknown statements, then they precede the TypeBodyStatement
    and UniqueStatement         = Unique            * ExtraStatements
    // TODO: Expand the Units type to map to standard units provided by F#
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

    and ModuleHeaderStatements      = YangVersionStatement * NamespaceStatement * PrefixStatement * (UnknownStatement list option)
    and SubmoduleHeaderStatements   = YangVersionStatement * BelongsToStatement * (UnknownStatement list option)
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
    and StatementPrinter ()         =
        class
            static let mutable StatementPrinterImplementation : (Statement -> string) option = None
            static member Set (printer : Statement -> string) = StatementPrinterImplementation <- Some printer
            static member Reset () = StatementPrinterImplementation <- None
            static member Print (st : Statement) =
                match StatementPrinterImplementation with
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

    /// Helper methods for the BitBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BitBodyStatement =

        let Translate = function
        | BitBodyStatement.IfFeature     st -> Statement.IfFeature      st
        | BitBodyStatement.Position      st -> Statement.Position       st
        | BitBodyStatement.Status        st -> Statement.Status         st
        | BitBodyStatement.Description   st -> Statement.Description    st
        | BitBodyStatement.Reference     st -> Statement.Reference      st
        | BitBodyStatement.Unknown       st -> Statement.Unknown        st

    /// Helper methods for the BodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BodyStatement =

        let Translate = function
        | BodyStatement.Extension       st -> Statement.Extension       st
        | BodyStatement.Feature         st -> Statement.Feature         st
        | BodyStatement.Identity        st -> Statement.Identity        st
        | BodyStatement.TypeDef         st -> Statement.TypeDef         st
        | BodyStatement.Grouping        st -> Statement.Grouping        st
        | BodyStatement.Container       st -> Statement.Container       st
        | BodyStatement.Leaf            st -> Statement.Leaf            st
        | BodyStatement.LeafList        st -> Statement.LeafList        st
        | BodyStatement.List            st -> Statement.List            st
        | BodyStatement.Choice          st -> Statement.Choice          st
        | BodyStatement.AnyData         st -> Statement.AnyData         st
        | BodyStatement.AnyXml          st -> Statement.AnyXml          st
        | BodyStatement.Uses            st -> Statement.Uses            st
        | BodyStatement.Augment         st -> Statement.Augment         st
        | BodyStatement.Rpc             st -> Statement.Rpc             st
        | BodyStatement.Notification    st -> Statement.Notification    st
        | BodyStatement.Deviation       st -> Statement.Deviation       st
        | BodyStatement.Unknown         st -> Statement.Unknown         st

        let IsContainer (this : BodyStatement) =
            match this with
            | BodyStatement.Container   _   -> true
            | _                             -> false

        let IsLeaf (this : BodyStatement) =
            match this with
            | BodyStatement.Leaf        _   -> true
            | _                             -> false

        let IsLeafList (this : BodyStatement) =
            match this with
            | BodyStatement.LeafList    _   -> true
            | _                             -> false


    /// Helper methods for the CaseBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CaseBodyStatement =

        let Translate = function
        | CaseBodyStatement.When          st -> Statement.When          st
        | CaseBodyStatement.IfFeature     st -> Statement.IfFeature     st
        | CaseBodyStatement.Status        st -> Statement.Status        st
        | CaseBodyStatement.Description   st -> Statement.Description   st
        | CaseBodyStatement.Reference     st -> Statement.Reference     st
        | CaseBodyStatement.Container     st -> Statement.Container     st
        | CaseBodyStatement.Leaf          st -> Statement.Leaf          st
        | CaseBodyStatement.LeafList      st -> Statement.LeafList      st
        | CaseBodyStatement.List          st -> Statement.List          st
        | CaseBodyStatement.Choice        st -> Statement.Choice        st
        | CaseBodyStatement.AnyData       st -> Statement.AnyData       st
        | CaseBodyStatement.AnyXml        st -> Statement.AnyXml        st
        | CaseBodyStatement.Uses          st -> Statement.Uses          st
        | CaseBodyStatement.Unknown       st -> Statement.Unknown       st

        let FromDataDefinition = function
        | BodyStatement.Container     st -> CaseBodyStatement.Container    st
        | BodyStatement.Leaf          st -> CaseBodyStatement.Leaf         st
        | BodyStatement.LeafList      st -> CaseBodyStatement.LeafList     st
        | BodyStatement.List          st -> CaseBodyStatement.List         st
        | BodyStatement.Choice        st -> CaseBodyStatement.Choice       st
        | BodyStatement.AnyData       st -> CaseBodyStatement.AnyData      st
        | BodyStatement.AnyXml        st -> CaseBodyStatement.AnyXml       st
        | BodyStatement.Uses          st -> CaseBodyStatement.Uses         st
        | _ as th -> raise (YangModelException (sprintf "Invalid transformation to type ContainerBodyStatement from %A" th))

    /// Helper methods for the ChoiceBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ChoiceBodyStatement =

        let Translate = function
        | ChoiceBodyStatement.When          st -> Statement.When        st
        | ChoiceBodyStatement.IfFeature     st -> Statement.IfFeature   st
        | ChoiceBodyStatement.Default       st -> Statement.Default     st
        | ChoiceBodyStatement.Config        st -> Statement.Config      st
        | ChoiceBodyStatement.Mandatory     st -> Statement.Mandatory   st
        | ChoiceBodyStatement.Status        st -> Statement.Status      st
        | ChoiceBodyStatement.Description   st -> Statement.Description st
        | ChoiceBodyStatement.Reference     st -> Statement.Reference   st
        | ChoiceBodyStatement.Choice        st -> Statement.Choice      st
        | ChoiceBodyStatement.Container     st -> Statement.Container   st
        | ChoiceBodyStatement.Leaf          st -> Statement.Leaf        st
        | ChoiceBodyStatement.LeafList      st -> Statement.LeafList    st
        | ChoiceBodyStatement.List          st -> Statement.List        st
        | ChoiceBodyStatement.AnyData       st -> Statement.AnyData     st
        | ChoiceBodyStatement.AnyXml        st -> Statement.AnyXml      st
        | ChoiceBodyStatement.Case          st -> Statement.Case        st
        | ChoiceBodyStatement.Unknown       st -> Statement.Unknown     st

    /// Helper methods for the ConfigStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ConfigStatement =
        let ValueAsString (this : ConfigStatement) =
            let (v, _) = this
            if v then "true" else "false"

    /// Helper methods for the ContainerBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ContainerBodyStatement =

        let Translate = function
        | ContainerBodyStatement.When          st -> Statement.When         st
        | ContainerBodyStatement.IfFeature     st -> Statement.IfFeature    st
        | ContainerBodyStatement.Must          st -> Statement.Must         st
        | ContainerBodyStatement.Presence      st -> Statement.Presence     st
        | ContainerBodyStatement.Config        st -> Statement.Config       st
        | ContainerBodyStatement.Status        st -> Statement.Status       st
        | ContainerBodyStatement.Description   st -> Statement.Description  st
        | ContainerBodyStatement.Reference     st -> Statement.Reference    st
        | ContainerBodyStatement.TypeDef       st -> Statement.TypeDef      st
        | ContainerBodyStatement.Grouping      st -> Statement.Grouping     st
        | ContainerBodyStatement.Container     st -> Statement.Container    st
        | ContainerBodyStatement.Leaf          st -> Statement.Leaf         st
        | ContainerBodyStatement.LeafList      st -> Statement.LeafList     st
        | ContainerBodyStatement.List          st -> Statement.List         st
        | ContainerBodyStatement.Choice        st -> Statement.Choice       st
        | ContainerBodyStatement.AnyData       st -> Statement.AnyData      st
        | ContainerBodyStatement.AnyXml        st -> Statement.AnyXml       st
        | ContainerBodyStatement.Uses          st -> Statement.Uses         st
        | ContainerBodyStatement.Action        st -> Statement.Action       st
        | ContainerBodyStatement.Notification  st -> Statement.Notification st
        | ContainerBodyStatement.Unknown       st -> Statement.Unknown      st

        let FromDataDefinition = function
        | BodyStatement.Container     st -> ContainerBodyStatement.Container    st
        | BodyStatement.Leaf          st -> ContainerBodyStatement.Leaf         st
        | BodyStatement.LeafList      st -> ContainerBodyStatement.LeafList     st
        | BodyStatement.List          st -> ContainerBodyStatement.List         st
        | BodyStatement.Choice        st -> ContainerBodyStatement.Choice       st
        | BodyStatement.AnyData       st -> ContainerBodyStatement.AnyData      st
        | BodyStatement.AnyXml        st -> ContainerBodyStatement.AnyXml       st
        | BodyStatement.Uses          st -> ContainerBodyStatement.Uses         st
        | _ as th -> raise (YangModelException (sprintf "Invalid transformation to type ContainerBodyStatement from %A" th))

        let IsContainer (this : ContainerBodyStatement) =
            match this with
            | ContainerBodyStatement.Container  _   -> true
            | _                                     -> false

        let IsLeaf (this : ContainerBodyStatement) =
            match this with
            | ContainerBodyStatement.Leaf       _   -> true
            | _                                     -> false

        let IsLeafList (this : ContainerBodyStatement) =
            match this with
            | ContainerBodyStatement.LeafList   _   -> true
            | _                                     -> false


    /// Helper methods for the DeviateAddBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DeviateAddBodyStatement =

        let Translate = function
        | DeviateAddBodyStatement.Units         st -> Statement.Units       st
        | DeviateAddBodyStatement.Must          st -> Statement.Must        st
        | DeviateAddBodyStatement.Unique        st -> Statement.Unique      st
        | DeviateAddBodyStatement.Default       st -> Statement.Default     st
        | DeviateAddBodyStatement.Config        st -> Statement.Config      st
        | DeviateAddBodyStatement.Mandatory     st -> Statement.Mandatory   st
        | DeviateAddBodyStatement.MinElements   st -> Statement.MinElements st
        | DeviateAddBodyStatement.MaxElements   st -> Statement.MaxElements st
        | DeviateAddBodyStatement.Unknown       st -> Statement.Unknown     st

    /// Helper methods for the DeviateDeleteBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DeviateDeleteBodyStatement =

        let Translate = function
        | DeviateDeleteBodyStatement.Units         st -> Statement.Units    st
        | DeviateDeleteBodyStatement.Must          st -> Statement.Must     st
        | DeviateDeleteBodyStatement.Unique        st -> Statement.Unique   st
        | DeviateDeleteBodyStatement.Default       st -> Statement.Default  st
        | DeviateDeleteBodyStatement.Unknown       st -> Statement.Unknown  st

    /// Helper methods for the DeviateReplaceBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DeviateReplaceBodyStatement =

        let Translate = function
        | DeviateReplaceBodyStatement.Type          st -> Statement.Type        st
        | DeviateReplaceBodyStatement.Units         st -> Statement.Units       st
        | DeviateReplaceBodyStatement.Default       st -> Statement.Default     st
        | DeviateReplaceBodyStatement.Config        st -> Statement.Config      st
        | DeviateReplaceBodyStatement.Mandatory     st -> Statement.Mandatory   st
        | DeviateReplaceBodyStatement.MinElements   st -> Statement.MinElements st
        | DeviateReplaceBodyStatement.MaxElements   st -> Statement.MaxElements st
        | DeviateReplaceBodyStatement.Unknown       st -> Statement.Unknown     st

    /// Helper methods for the DeviationBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DeviationBodyStatement =

        let Translate = function
        | DeviationBodyStatement.Description            st -> Statement.Description         st
        | DeviationBodyStatement.Reference              st -> Statement.Reference           st
        | DeviationBodyStatement.DeviateNotSupported    st -> Statement.DeviateNotSupported st
        | DeviationBodyStatement.DeviateAdd             st -> Statement.DeviateAdd          st
        | DeviationBodyStatement.DeviateReplace         st -> Statement.DeviateReplace      st
        | DeviationBodyStatement.DeviateDelete          st -> Statement.DeviateDelete       st
        | DeviationBodyStatement.Unknown                st -> Statement.Unknown             st

    /// Helper methods for the EnumBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module EnumBodyStatement =

        let Translate = function
        | EnumBodyStatement.IfFeature     st -> Statement.IfFeature     st
        | EnumBodyStatement.Value         st -> Statement.Value         st
        | EnumBodyStatement.Status        st -> Statement.Status        st
        | EnumBodyStatement.Description   st -> Statement.Description   st
        | EnumBodyStatement.Reference     st -> Statement.Reference     st
        | EnumBodyStatement.Unknown       st -> Statement.Unknown       st

    /// Helper methods for the ExtensionBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ExtensionBodyStatement =

        let Translate = function
        | ExtensionBodyStatement.Argument      st -> Statement.Argument     st
        | ExtensionBodyStatement.Status        st -> Statement.Status       st
        | ExtensionBodyStatement.Description   st -> Statement.Description  st
        | ExtensionBodyStatement.Reference     st -> Statement.Reference    st
        | ExtensionBodyStatement.Unknown       st -> Statement.Unknown      st

    /// Helper methods for the FeatureBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FeatureBodyStatement =

        let Translate = function
        | FeatureBodyStatement.IfFeature     st -> Statement.IfFeature      st
        | FeatureBodyStatement.Status        st -> Statement.Status         st
        | FeatureBodyStatement.Description   st -> Statement.Description    st
        | FeatureBodyStatement.Reference     st -> Statement.Reference      st
        | FeatureBodyStatement.Unknown       st -> Statement.Unknown        st

    /// Helper methods for the GroupingBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module GroupingBodyStatement =

        let Translate = function
        | GroupingBodyStatement.Status        st -> Statement.Status        st
        | GroupingBodyStatement.Description   st -> Statement.Description   st
        | GroupingBodyStatement.Reference     st -> Statement.Reference     st
        | GroupingBodyStatement.TypeDef       st -> Statement.TypeDef       st
        | GroupingBodyStatement.Grouping      st -> Statement.Grouping      st
        | GroupingBodyStatement.Container     st -> Statement.Container     st
        | GroupingBodyStatement.Leaf          st -> Statement.Leaf          st
        | GroupingBodyStatement.LeafList      st -> Statement.LeafList      st
        | GroupingBodyStatement.List          st -> Statement.List          st
        | GroupingBodyStatement.Choice        st -> Statement.Choice        st
        | GroupingBodyStatement.AnyData       st -> Statement.AnyData       st
        | GroupingBodyStatement.AnyXml        st -> Statement.AnyXml        st
        | GroupingBodyStatement.Uses          st -> Statement.Uses          st
        | GroupingBodyStatement.Action        st -> Statement.Action        st
        | GroupingBodyStatement.Notification  st -> Statement.Notification  st
        | GroupingBodyStatement.Unknown       st -> Statement.Unknown       st

        let FromDataDefinition = function
        | BodyStatement.Container     st -> GroupingBodyStatement.Container     st
        | BodyStatement.Leaf          st -> GroupingBodyStatement.Leaf          st
        | BodyStatement.LeafList      st -> GroupingBodyStatement.LeafList      st
        | BodyStatement.List          st -> GroupingBodyStatement.List          st
        | BodyStatement.Choice        st -> GroupingBodyStatement.Choice        st
        | BodyStatement.AnyData       st -> GroupingBodyStatement.AnyData       st
        | BodyStatement.AnyXml        st -> GroupingBodyStatement.AnyXml        st
        | BodyStatement.Uses          st -> GroupingBodyStatement.Uses          st
        | _ as th -> raise (YangModelException (sprintf "Invalid transformation to type GroupingBodyStatement from %A" th))

    /// Helper methods for the IdentityBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module IdentityBodyStatement =

        let Translate = function
        | IdentityBodyStatement.IfFeature     st -> Statement.IfFeature     st
        | IdentityBodyStatement.Base          st -> Statement.Base          st
        | IdentityBodyStatement.Status        st -> Statement.Status        st
        | IdentityBodyStatement.Description   st -> Statement.Description   st
        | IdentityBodyStatement.Reference     st -> Statement.Reference     st
        | IdentityBodyStatement.Unknown       st -> Statement.Unknown       st

    /// Helper methods for the ImportBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ImportBodyStatement =

        let Translate = function
        | ImportBodyStatement.Prefix        st -> Statement.Prefix          st
        | ImportBodyStatement.RevisionDate  st -> Statement.RevisionDate    st
        | ImportBodyStatement.Description   st -> Statement.Description     st
        | ImportBodyStatement.Reference     st -> Statement.Reference       st
        | ImportBodyStatement.Unknown       st -> Statement.Unknown         st

    /// Helper methods for the IncludeBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module IncludeBodyStatement =

        let Translate = function
        | IncludeBodyStatement.RevisionDate  st -> Statement.RevisionDate   st
        | IncludeBodyStatement.Description   st -> Statement.Description    st
        | IncludeBodyStatement.Reference     st -> Statement.Reference      st
        | IncludeBodyStatement.Unknown       st -> Statement.Unknown        st

    /// Helper methods for the InputBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module InputBodyStatement =

        let FromDataDefinition = function
        | BodyStatement.Container     st -> InputBodyStatement.Container    st
        | BodyStatement.Leaf          st -> InputBodyStatement.Leaf         st
        | BodyStatement.LeafList      st -> InputBodyStatement.LeafList     st
        | BodyStatement.List          st -> InputBodyStatement.List         st
        | BodyStatement.Choice        st -> InputBodyStatement.Choice       st
        | BodyStatement.AnyData       st -> InputBodyStatement.AnyData      st
        | BodyStatement.AnyXml        st -> InputBodyStatement.AnyXml       st
        | BodyStatement.Uses          st -> InputBodyStatement.Uses         st
        | _ as th -> raise (YangModelException (sprintf "Invalid transformation to type InputBodyStatement from %A" th))

        let Translate = function
        | InputBodyStatement.Must          st -> Statement.Must             st
        | InputBodyStatement.TypeDef       st -> Statement.TypeDef          st
        | InputBodyStatement.Grouping      st -> Statement.Grouping         st
        | InputBodyStatement.Container     st -> Statement.Container        st
        | InputBodyStatement.Leaf          st -> Statement.Leaf             st
        | InputBodyStatement.LeafList      st -> Statement.LeafList         st
        | InputBodyStatement.List          st -> Statement.List             st
        | InputBodyStatement.Choice        st -> Statement.Choice           st
        | InputBodyStatement.AnyData       st -> Statement.AnyData          st
        | InputBodyStatement.AnyXml        st -> Statement.AnyXml           st
        | InputBodyStatement.Uses          st -> Statement.Uses             st
        | InputBodyStatement.Unknown       st -> Statement.Unknown          st

    /// Helper methods for the LeafBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LeafBodyStatement =

        let Translate = function
        | LeafBodyStatement.When          st -> Statement.When              st
        | LeafBodyStatement.IfFeature     st -> Statement.IfFeature         st
        | LeafBodyStatement.Type          st -> Statement.Type              st
        | LeafBodyStatement.Units         st -> Statement.Units             st
        | LeafBodyStatement.Must          st -> Statement.Must              st
        | LeafBodyStatement.Default       st -> Statement.Default           st
        | LeafBodyStatement.Config        st -> Statement.Config            st
        | LeafBodyStatement.Mandatory     st -> Statement.Mandatory         st
        | LeafBodyStatement.Status        st -> Statement.Status            st
        | LeafBodyStatement.Description   st -> Statement.Description       st
        | LeafBodyStatement.Reference     st -> Statement.Reference         st
        | LeafBodyStatement.Unknown       st -> Statement.Unknown           st

        let IsType        = function
        | LeafBodyStatement.Type _          -> true
        | _                                 -> false

        let IsDescription = function
        | LeafBodyStatement.Description _   -> true
        | _                                 -> false

    /// Helper methods for the LeafListBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LeafListBodyStatement =

        let Translate = function
        | LeafListBodyStatement.When          st -> Statement.When          st
        | LeafListBodyStatement.IfFeature     st -> Statement.IfFeature     st
        | LeafListBodyStatement.Type          st -> Statement.Type          st
        | LeafListBodyStatement.Units         st -> Statement.Units         st
        | LeafListBodyStatement.Must          st -> Statement.Must          st
        | LeafListBodyStatement.Default       st -> Statement.Default       st
        | LeafListBodyStatement.Config        st -> Statement.Config        st
        | LeafListBodyStatement.MinElements   st -> Statement.MinElements   st
        | LeafListBodyStatement.MaxElements   st -> Statement.MaxElements   st
        | LeafListBodyStatement.OrderedBy     st -> Statement.OrderedBy     st
        | LeafListBodyStatement.Status        st -> Statement.Status        st
        | LeafListBodyStatement.Description   st -> Statement.Description   st
        | LeafListBodyStatement.Reference     st -> Statement.Reference     st
        | LeafListBodyStatement.Unknown       st -> Statement.Unknown       st

        let IsDescription = function
        | LeafListBodyStatement.Description _   -> true
        | _                                     -> false

        let IsType = function
        | LeafListBodyStatement.Type        _   -> true
        | _                                     -> false

    /// Helper methods for the LeafListBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LeafListStatement =
        let private try_find filter (this : LeafListStatement) =
            let _, statements = this
            statements |> List.tryFind filter
            
        let Description = try_find LeafListBodyStatement.IsDescription
        let Type = try_find LeafListBodyStatement.IsType

        let Identifier (this : LeafListStatement) = let (id, _) = this in id
        let IdentifierAsString (this : LeafListStatement) = let (id, _) = this in id.Value

        let Statements (this : LeafListStatement) = let (_, st) = this in st


    /// Helper methods for the LeafStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LeafStatement =
        let private try_find filter (this : LeafStatement) =
            let _, statements = this
            statements |> List.tryFind filter
            
        let Description = try_find LeafBodyStatement.IsDescription
        let Type = try_find LeafBodyStatement.IsType

        let Identifier (this : LeafStatement) = let (id, _) = this in id
        let IdentifierAsString (this : LeafStatement) = let (id, _) = this in id.Value

        let Statements (this : LeafStatement) = let (_, st) = this in st


    /// Helper methods for the LengthBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LengthBodyStatement =

        let Translate = function
        | LengthBodyStatement.ErrorMessage  st -> Statement.ErrorMessage    st
        | LengthBodyStatement.ErrorAppTag   st -> Statement.ErrorAppTag     st
        | LengthBodyStatement.Description   st -> Statement.Description     st
        | LengthBodyStatement.Reference     st -> Statement.Reference       st
        | LengthBodyStatement.Unknown       st -> Statement.Unknown         st

    /// Helper methods for the LinkageBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LinkageBodyStatement =

        let Translate = function
        | LinkageBodyStatement.Import   st -> Statement.Import  st
        | LinkageBodyStatement.Include  st -> Statement.Include st

    /// Helper methods for the ListBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ListBodyStatement =

        let Translate = function
        | ListBodyStatement.When          st -> Statement.When          st
        | ListBodyStatement.IfFeature     st -> Statement.IfFeature     st
        | ListBodyStatement.Must          st -> Statement.Must          st
        | ListBodyStatement.Key           st -> Statement.Key           st
        | ListBodyStatement.Unique        st -> Statement.Unique        st
        | ListBodyStatement.Config        st -> Statement.Config        st
        | ListBodyStatement.MinElements   st -> Statement.MinElements   st
        | ListBodyStatement.MaxElements   st -> Statement.MaxElements   st
        | ListBodyStatement.OrderedBy     st -> Statement.OrderedBy     st
        | ListBodyStatement.Status        st -> Statement.Status        st
        | ListBodyStatement.Description   st -> Statement.Description   st
        | ListBodyStatement.Reference     st -> Statement.Reference     st
        | ListBodyStatement.TypeDef       st -> Statement.TypeDef       st
        | ListBodyStatement.Grouping      st -> Statement.Grouping      st
        | ListBodyStatement.Container     st -> Statement.Container     st
        | ListBodyStatement.Leaf          st -> Statement.Leaf          st
        | ListBodyStatement.LeafList      st -> Statement.LeafList      st
        | ListBodyStatement.List          st -> Statement.List          st
        | ListBodyStatement.Choice        st -> Statement.Choice        st
        | ListBodyStatement.AnyData       st -> Statement.AnyData       st
        | ListBodyStatement.AnyXml        st -> Statement.AnyXml        st
        | ListBodyStatement.Uses          st -> Statement.Uses          st
        | ListBodyStatement.Action        st -> Statement.Action        st
        | ListBodyStatement.Notification  st -> Statement.Notification  st
        | ListBodyStatement.Unknown       st -> Statement.Unknown       st

        let FromDataDefinition = function
        | BodyStatement.Container     st -> ListBodyStatement.Container    st
        | BodyStatement.Leaf          st -> ListBodyStatement.Leaf         st
        | BodyStatement.LeafList      st -> ListBodyStatement.LeafList     st
        | BodyStatement.List          st -> ListBodyStatement.List         st
        | BodyStatement.Choice        st -> ListBodyStatement.Choice       st
        | BodyStatement.AnyData       st -> ListBodyStatement.AnyData      st
        | BodyStatement.AnyXml        st -> ListBodyStatement.AnyXml       st
        | BodyStatement.Uses          st -> ListBodyStatement.Uses         st
        | _ as th -> raise (YangModelException (sprintf "Invalid transformation to type ContainerBodyStatement from %A" th))

    /// Helper methods for the MetaBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MetaBodyStatement =

        let IsOrganization = function
        | MetaBodyStatement.Organization    _   -> true
        | _                                     -> false

        let IsContact = function
        | MetaBodyStatement.Contact         _   -> true
        | _                                     -> false

        let IsDescription = function
        | MetaBodyStatement.Description     _   -> true
        | _                                     -> false

        let IsReference = function
        | MetaBodyStatement.Reference       _   -> true
        | _                                     -> false

        let IsUnknown = function
        | MetaBodyStatement.Unknown         _   -> true
        | _                                     -> false

        let Translate = function
        | MetaBodyStatement.Organization  st -> Statement.Organization  st
        | MetaBodyStatement.Contact       st -> Statement.Contact       st
        | MetaBodyStatement.Description   st -> Statement.Description   st
        | MetaBodyStatement.Reference     st -> Statement.Reference     st
        | MetaBodyStatement.Unknown       st -> Statement.Unknown       st

    /// Helper methods for the MetaBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MetaStatements =
        let private try_find filter (this : MetaStatements) = this |> List.tryFind filter

        /// Get the organization meta information; None if not exists
        let Organization (this : MetaStatements) = 
            try_find MetaBodyStatement.IsOrganization this
            |> Option.bind (fun s -> match s with | MetaBodyStatement.Organization o -> Some o | _ -> None)

        /// Get the organization meta information; None if not exists
        let Contact      (this : MetaStatements) =
            try_find MetaBodyStatement.IsContact this
            |> Option.bind (fun s -> match s with | MetaBodyStatement.Contact o -> Some o | _ -> None)

        /// Get the description meta information; None if not exists
        let Description  (this : MetaStatements) =
            try_find MetaBodyStatement.IsDescription this
            |> Option.bind (fun s -> match s with | MetaBodyStatement.Description o -> Some o | _ -> None)

        /// Get the reference meta information; None if not exists
        let Reference    (this : MetaStatements) =
            try_find MetaBodyStatement.IsReference this
            |> Option.bind (fun s -> match s with | MetaBodyStatement.Reference o -> Some o | _ -> None)

        /// Get the unknown statements that have been associated with the meta section
        let Unknown      (this : MetaStatements) =
            this
            |> List.choose (
                fun st ->
                    match st with
                    | MetaBodyStatement.Unknown st' -> Some st'
                    | _                             -> None
            )
            |> (fun l -> if l.Length = 0 then None else Some l)

        // Returns an empty list of meta-statements
        let Empty : MetaStatements = []

    /// Helper methods for the ModuleStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ModuleStatement =
        let BodyAsStatement (this : ModuleStatement) =
            this.Body
            |> List.map (BodyStatement.Translate)

    /// Helper methods for the MustBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MustBodyStatement =

        let Translate = function
        | MustBodyStatement.ErrorMessage  st -> Statement.ErrorMessage  st
        | MustBodyStatement.ErrorAppTag   st -> Statement.ErrorAppTag   st
        | MustBodyStatement.Description   st -> Statement.Description   st
        | MustBodyStatement.Reference     st -> Statement.Reference     st
        | MustBodyStatement.Unknown       st -> Statement.Unknown       st

    /// Helper methods for the NotificationBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module NotificationBodyStatement =

        let Translate = function
        | NotificationBodyStatement.IfFeature     st -> Statement.IfFeature     st
        | NotificationBodyStatement.Must          st -> Statement.Must          st
        | NotificationBodyStatement.Status        st -> Statement.Status        st
        | NotificationBodyStatement.Description   st -> Statement.Description   st
        | NotificationBodyStatement.Reference     st -> Statement.Reference     st
        | NotificationBodyStatement.TypeDef       st -> Statement.TypeDef       st
        | NotificationBodyStatement.Grouping      st -> Statement.Grouping      st
        | NotificationBodyStatement.Container     st -> Statement.Container     st
        | NotificationBodyStatement.Leaf          st -> Statement.Leaf          st
        | NotificationBodyStatement.LeafList      st -> Statement.LeafList      st
        | NotificationBodyStatement.List          st -> Statement.List          st
        | NotificationBodyStatement.Choice        st -> Statement.Choice        st
        | NotificationBodyStatement.AnyData       st -> Statement.AnyData       st
        | NotificationBodyStatement.AnyXml        st -> Statement.AnyXml        st
        | NotificationBodyStatement.Uses          st -> Statement.Uses          st
        | NotificationBodyStatement.Unknown       st -> Statement.Unknown       st

    /// Helper methods for the OutputBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module OutputBodyStatement =

        let FromDataDefinition = function
        | BodyStatement.Container     st -> OutputBodyStatement.Container    st
        | BodyStatement.Leaf          st -> OutputBodyStatement.Leaf         st
        | BodyStatement.LeafList      st -> OutputBodyStatement.LeafList     st
        | BodyStatement.List          st -> OutputBodyStatement.List         st
        | BodyStatement.Choice        st -> OutputBodyStatement.Choice       st
        | BodyStatement.AnyData       st -> OutputBodyStatement.AnyData      st
        | BodyStatement.AnyXml        st -> OutputBodyStatement.AnyXml       st
        | BodyStatement.Uses          st -> OutputBodyStatement.Uses         st
        | _ as th -> raise (YangModelException (sprintf "Invalid transformation to type InputBodyStatement from %A" th))


        let Translate = function
        | OutputBodyStatement.Must          st -> Statement.Must        st
        | OutputBodyStatement.TypeDef       st -> Statement.TypeDef     st
        | OutputBodyStatement.Grouping      st -> Statement.Grouping    st
        | OutputBodyStatement.Container     st -> Statement.Container   st
        | OutputBodyStatement.Leaf          st -> Statement.Leaf        st
        | OutputBodyStatement.LeafList      st -> Statement.LeafList    st
        | OutputBodyStatement.List          st -> Statement.List        st
        | OutputBodyStatement.Choice        st -> Statement.Choice      st
        | OutputBodyStatement.AnyData       st -> Statement.AnyData     st
        | OutputBodyStatement.AnyXml        st -> Statement.AnyXml      st
        | OutputBodyStatement.Uses          st -> Statement.Uses        st
        | OutputBodyStatement.Unknown       st -> Statement.Unknown     st

    /// Helper methods for the PatternBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module PatternBodyStatement =

        let Translate = function
        | PatternBodyStatement.Modifier      st -> Statement.Modifier       st
        | PatternBodyStatement.ErrorMessage  st -> Statement.ErrorMessage   st
        | PatternBodyStatement.ErrorAppTag   st -> Statement.ErrorAppTag    st
        | PatternBodyStatement.Description   st -> Statement.Description    st
        | PatternBodyStatement.Reference     st -> Statement.Reference      st
        | PatternBodyStatement.Unknown       st -> Statement.Unknown        st

    /// Helper methods for the RangeBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RangeBodyStatement =

        let Translate = function
        | RangeBodyStatement.ErrorMessage  st -> Statement.ErrorMessage st
        | RangeBodyStatement.ErrorAppTag   st -> Statement.ErrorAppTag  st
        | RangeBodyStatement.Description   st -> Statement.Description  st
        | RangeBodyStatement.Reference     st -> Statement.Reference    st
        | RangeBodyStatement.Unknown       st -> Statement.Unknown      st

    /// Helper methods for the RefineBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RefineBodyStatement =

        let Translate = function
        | RefineBodyStatement.IfFeature     st -> Statement.IfFeature   st
        | RefineBodyStatement.Must          st -> Statement.Must        st
        | RefineBodyStatement.Presence      st -> Statement.Presence    st
        | RefineBodyStatement.Default       st -> Statement.Default     st
        | RefineBodyStatement.Config        st -> Statement.Config      st
        | RefineBodyStatement.Mandatory     st -> Statement.Mandatory   st
        | RefineBodyStatement.MinElements   st -> Statement.MinElements st
        | RefineBodyStatement.MaxElements   st -> Statement.MaxElements st
        | RefineBodyStatement.Description   st -> Statement.Description st
        | RefineBodyStatement.Reference     st -> Statement.Reference   st
        | RefineBodyStatement.Unknown       st -> Statement.Unknown     st

    /// Helper methods for the RevisionBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RevisionBodyStatement =

        let Translate = function
        | RevisionBodyStatement.Description   st -> Statement.Description   st
        | RevisionBodyStatement.Reference     st -> Statement.Reference     st
        | RevisionBodyStatement.Unknown       st -> Statement.Unknown       st

        let IsDescription = function
        | RevisionBodyStatement.Description _   -> true
        | _                                     -> false

        let IsReference = function
        | RevisionBodyStatement.Reference _     -> true
        | _                                     -> false

        let IsUnknown = function
        | RevisionBodyStatement.Unknown _       -> true
        | _                                     -> false

    /// Helper methods for the RevisionStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RevisionStatement =
        let private try_find filter = function
        | _, Some statements    -> statements |> List.tryFind filter
        | _, _                  -> None

        let private try_find_all filter = function
        | _, Some statements    ->
            let statements' = statements |> List.filter filter
            if statements'.Length = 0 then None else Some statements'
        | _, _                  -> None

        let Version (this : RevisionStatement) = let (v, _) = this in v

        let Description (this : RevisionStatement) =
            try_find RevisionBodyStatement.IsDescription this
            |> Option.bind (fun o -> match o with | RevisionBodyStatement.Description d -> Some d | _ -> None)

        let Reference   (this : RevisionStatement) =
            try_find RevisionBodyStatement.IsReference this
            |> Option.bind (fun o -> match o with | RevisionBodyStatement.Reference r -> Some r | _ -> None)

        let Unknown     (this : RevisionStatement) =
            try_find_all RevisionBodyStatement.IsUnknown this
            |> Option.map (List.choose (fun o -> match o with | RevisionBodyStatement.Unknown u -> Some u | _ -> None))

    /// Helper methods for the RpcBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RpcBodyStatement =

        let Translate = function
        | RpcBodyStatement.IfFeature     st -> Statement.IfFeature st
        | RpcBodyStatement.Status        st -> Statement.Status st
        | RpcBodyStatement.Description   st -> Statement.Description st
        | RpcBodyStatement.Reference     st -> Statement.Reference st
        | RpcBodyStatement.TypeDef       st -> Statement.TypeDef st
        | RpcBodyStatement.Grouping      st -> Statement.Grouping st
        | RpcBodyStatement.Input         st -> Statement.Input st
        | RpcBodyStatement.Output        st -> Statement.Output st
        | RpcBodyStatement.Unknown       st -> Statement.Unknown st

    /// Helper methods for the SubmoduleStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SubmoduleStatement =
        let BodyAsStatement (this : SubmoduleStatement) =
            this.Body
            |> List.map (BodyStatement.Translate)

    /// Helper methods for the TypeBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TypeBodyStatement =
        open System.Text

        let Print (sb : StringBuilder, _indentation : int) = function
        // TODO: Fix printing of type restrictions and specifications
        | TypeBodyStatement.NumericalRestrictions    rs                     ->
            Printf.bprintf sb "%A" rs
        | TypeBodyStatement.Decimal64Specification  (fraction, None)        ->
            Printf.bprintf sb "%A" fraction
        | TypeBodyStatement.Decimal64Specification  (fraction, Some range)  ->
            Printf.bprintf sb "%A %A" fraction range
        | TypeBodyStatement.StringRestrictions      (None, [])              ->
            ()
        | TypeBodyStatement.StringRestrictions      (Some length, [])       ->
            Printf.bprintf sb "%A" length
        | TypeBodyStatement.StringRestrictions      (None, patterns)        ->
            patterns |> List.iter (fun pattern -> Printf.bprintf sb "%A " pattern)
        | TypeBodyStatement.StringRestrictions      (Some length, patterns) ->
            Printf.bprintf sb "%A" length
            patterns |> List.iter (fun pattern -> Printf.bprintf sb "%A " pattern)
        | TypeBodyStatement.EnumSpecification        enums                  ->
            enums |> List.iter (fun enum -> Printf.bprintf sb "%A " enum)
        | TypeBodyStatement.LeafRefSpecification    (path, None)            ->
            Printf.bprintf sb "%A" path
        | TypeBodyStatement.LeafRefSpecification    (path, requires)        ->
            Printf.bprintf sb "%A %A" path requires
        | TypeBodyStatement.IdentityRefSpecification statements             ->
            statements |> List.iter (fun statement -> Printf.bprintf sb "%A " statement)
        | TypeBodyStatement.InstanceIdentifierSpecification     None        ->
            ()
        | TypeBodyStatement.InstanceIdentifierSpecification     requires    ->
            Printf.bprintf sb "%A" requires
        | TypeBodyStatement.BitsSpecification statements ->
            statements |> List.iter (fun statement -> Printf.bprintf sb "%A " statement)
        | TypeBodyStatement.UnionSpecification          statements ->
            statements |> List.iter (fun statement -> Printf.bprintf sb "%A " statement)
        | TypeBodyStatement.BinarySpecification  None ->
            ()
        | TypeBodyStatement.BinarySpecification  (Some length) ->
            Printf.bprintf sb "%A" length

    /// Helper methods for the TypeDefBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TypeDefBodyStatement =

        let Translate = function
        | TypeDefBodyStatement.Type          st -> Statement.Type           st
        | TypeDefBodyStatement.Units         st -> Statement.Units          st
        | TypeDefBodyStatement.Default       st -> Statement.Default        st
        | TypeDefBodyStatement.Status        st -> Statement.Status         st
        | TypeDefBodyStatement.Description   st -> Statement.Description    st
        | TypeDefBodyStatement.Reference     st -> Statement.Reference      st
        | TypeDefBodyStatement.Unknown       st -> Statement.Unknown        st

    /// Helper methods for the UnknownStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module UnknownStatement =
        let Translate this = Statement.Unknown this

    /// Helper methods for the UsesBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module UsesBodyStatement =

        let Translate = function
        | UsesBodyStatement.When          st -> Statement.When st
        | UsesBodyStatement.IfFeature     st -> Statement.IfFeature st
        | UsesBodyStatement.Status        st -> Statement.Status st
        | UsesBodyStatement.Description   st -> Statement.Description st
        | UsesBodyStatement.Reference     st -> Statement.Reference st
        | UsesBodyStatement.Refine        st -> Statement.Refine st
        | UsesBodyStatement.UsesAugment   st -> Statement.UsesAugment st
        | UsesBodyStatement.Unknown       st -> Statement.Unknown st

    /// Helper methods for the UsesAugmentBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module UsesAugmentBodyStatement =

        let Translate = function
        | UsesAugmentBodyStatement.When          st -> Statement.When           st
        | UsesAugmentBodyStatement.IfFeature     st -> Statement.IfFeature      st
        | UsesAugmentBodyStatement.Status        st -> Statement.Status         st
        | UsesAugmentBodyStatement.Description   st -> Statement.Description    st
        | UsesAugmentBodyStatement.Reference     st -> Statement.Reference      st
        | UsesAugmentBodyStatement.Container     st -> Statement.Container      st
        | UsesAugmentBodyStatement.Leaf          st -> Statement.Leaf           st
        | UsesAugmentBodyStatement.LeafList      st -> Statement.LeafList       st
        | UsesAugmentBodyStatement.List          st -> Statement.List           st
        | UsesAugmentBodyStatement.Choice        st -> Statement.Choice         st
        | UsesAugmentBodyStatement.AnyData       st -> Statement.AnyData        st
        | UsesAugmentBodyStatement.AnyXml        st -> Statement.AnyXml         st
        | UsesAugmentBodyStatement.Uses          st -> Statement.Uses           st
        | UsesAugmentBodyStatement.Case          st -> Statement.Case           st
        | UsesAugmentBodyStatement.Action        st -> Statement.Action         st
        | UsesAugmentBodyStatement.Notification  st -> Statement.Notification   st
        | UsesAugmentBodyStatement.Unknown       st -> Statement.Unknown        st

    /// Helper methods for the WhenBodyStatement type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module WhenBodyStatement =

        let Translate = function
        | WhenBodyStatement.Description   st -> Statement.Description   st
        | WhenBodyStatement.Reference     st -> Statement.Reference     st
        | WhenBodyStatement.Unknown       st -> Statement.Unknown       st

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

            | Statement.Unknown _   -> None

        /// Get the YANG keyword used to define the statement
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
