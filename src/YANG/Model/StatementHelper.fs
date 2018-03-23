// StatementHelper.fs
// Helper methods for reasoning about statements
namespace Yang.Model

module StatementHelper =

    let private get_inner_optional (statements : 'T list option) (map : 'T -> Statement) : Statement list =
        if statements.IsSome then statements.Value |> List.map map
        else []

    let private get_inner (statements : 'T list) (map : 'T -> Statement) : Statement list =
        statements |> List.map map

    let Children : Statement -> Statement list = function
    | Statement.Base                (BaseStatement          (_, options))
    | Statement.Config              (ConfigStatement        (_, options))
    | Statement.Contact             (ContactStatement       (_, options))
    | Statement.Default             (DefaultStatement       (_, options))
    | Statement.Description         (DescriptionStatement   (_, options))
    | Statement.DeviateNotSupported (DeviateNotSupportedStatement options)
    | Statement.ErrorAppTag         (ErrorAppTagStatement   (_, options))
    | Statement.ErrorMessage        (ErrorMessageStatement  (_, options))
    | Statement.FractionDigits      (FractionDigitsStatement(_, options))
    | Statement.IfFeature           (IfFeatureStatement     (_, options))
    | Statement.Key                 (KeyStatement           (_, options))
    | Statement.Mandatory           (MandatoryStatement     (_, options))
    | Statement.MaxElements         (MaxElementsStatement   (_, options))
    | Statement.MinElements         (MinElementsStatement   (_, options))
    | Statement.Modifier            (ModifierStatement      (_, options))
    | Statement.Namespace           (NamespaceStatement     (_, options))
    | Statement.OrderedBy           (OrderedByStatement     (_, options))
    | Statement.Organization        (OrganizationStatement  (_, options))
    | Statement.Path                (PathStatement          (_, options))
    | Statement.Position            (PositionStatement      (_, options))
    | Statement.Prefix              (PrefixStatement        (_, options))
    | Statement.Presence            (PresenceStatement      (_, options))
    | Statement.Reference           (ReferenceStatement     (_, options))
    | Statement.RevisionDate        (RevisionDateStatement  (_, options))
    | Statement.RequireInstance     (RequireInstanceStatement (_, options))
    | Statement.Status              (StatusStatement        (_, options))
    | Statement.Units               (UnitsStatement         (_, options))
    | Statement.Unique              (UniqueStatement        (_, options))
    | Statement.Value               (ValueStatement         (_, options))
    | Statement.YangVersion         (YangVersionStatement   (_, options))
    | Statement.YinElement          (YinElementStatement    (_, options))
        -> if options.IsSome then options.Value else []

    | Statement.Action          (ActionStatement        (_, body))   -> get_inner_optional body ActionBodyStatement.Translate
    | Statement.AnyData         (AnyDataStatement       (_, body))   -> get_inner_optional body AnyDataBodyStatement.Translate
    | Statement.AnyXml          (AnyXmlStatement        (_, body))   -> get_inner_optional body AnyXmlBodyStatement.Translate
    | Statement.Augment         (AugmentStatement       (_, body))   -> get_inner          body AugmentBodyStatement.Translate
    | Statement.Argument        (ArgumentStatement      (_, body))   -> get_inner_optional body ArgumentBodyStatement.Translate
    | Statement.BelongsTo       (BelongsToStatement     (_, body))   -> get_inner          body BelongsToBodyStatement.Translate
    | Statement.Bit             (BitStatement           (_, body))   -> get_inner_optional body BitBodyStatement.Translate
    | Statement.Case            (CaseStatement          (_, body))   -> get_inner_optional body CaseBodyStatement.Translate
    | Statement.Choice          (ChoiceStatement        (_, body))   -> get_inner_optional body ChoiceBodyStatement.Translate
    | Statement.Container       (ContainerStatement     (_, body))   -> get_inner_optional body ContainerBodyStatement.Translate
    | Statement.DeviateAdd      (DeviateAddStatement        body)    -> get_inner_optional body DeviateAddBodyStatement.Translate
    | Statement.DeviateDelete   (DeviateDeleteStatement     body)    -> get_inner_optional body DeviateDeleteBodyStatement.Translate
    | Statement.DeviateReplace  (DeviateReplaceStatement    body)    -> get_inner_optional body DeviateReplaceBodyStatement.Translate
    | Statement.Deviation       (DeviationStatement     (_, body))   -> get_inner          body DeviationBodyStatement.Translate
    | Statement.Enum            (EnumStatement          (_, body))   -> get_inner_optional body EnumBodyStatement.Translate
    | Statement.Extension       (ExtensionStatement     (_, body))   -> get_inner_optional body ExtensionBodyStatement.Translate
    | Statement.Feature         (FeatureStatement       (_, body))   -> get_inner_optional body FeatureBodyStatement.Translate
    | Statement.Grouping        (GroupingStatement      (_, body))   -> get_inner_optional body GroupingBodyStatement.Translate
    | Statement.Identity        (IdentityStatement      (_, body))   -> get_inner_optional body IdentityBodyStatement.Translate
    | Statement.Import          (ImportStatement        (_, body))   -> get_inner          body ImportBodyStatement.Translate
    | Statement.Include         (IncludeStatement       (_, body))   -> get_inner_optional body IncludeBodyStatement.Translate
    | Statement.Input           (InputStatement             body)    -> get_inner          body InputBodyStatement.Translate
    | Statement.Leaf            (LeafStatement          (_, body))   -> get_inner          body LeafBodyStatement.Translate
    | Statement.LeafList        (LeafListStatement      (_, body))   -> get_inner          body LeafListBodyStatement.Translate
    | Statement.Length          (LengthStatement        (_, body))   -> get_inner_optional body LengthBodyStatement.Translate
    | Statement.List            (ListStatement          (_, body))   -> get_inner          body ListBodyStatement.Translate
    | Statement.Must            (MustStatement          (_, body))   -> get_inner_optional body MustBodyStatement.Translate
    | Statement.Notification    (NotificationStatement  (_, body))   -> get_inner_optional body NotificationBodyStatement.Translate
    | Statement.Output          (OutputStatement            body)    -> get_inner          body OutputBodyStatement.Translate
    | Statement.Pattern         (PatternStatement       (_, body))   -> get_inner_optional body PatternBodyStatement.Translate
    | Statement.Range           (RangeStatement         (_, body))   -> get_inner_optional body RangeBodyStatement.Translate
    | Statement.Rpc             (RpcStatement           (_, body))   -> get_inner_optional body RpcBodyStatement.Translate
    | Statement.Refine          (RefineStatement        (_, body))   -> get_inner_optional body RefineBodyStatement.Translate
    | Statement.Revision        (RevisionStatement      (_, body))   -> get_inner_optional body RevisionBodyStatement.Translate
    | Statement.TypeDef         (TypeDefStatement       (_, body))   -> get_inner          body TypeDefBodyStatement.Translate
    | Statement.Uses            (UsesStatement          (_, body))   -> get_inner_optional body UsesBodyStatement.Translate
    | Statement.UsesAugment     (UsesAugmentStatement   (_, body))   -> get_inner          body UsesAugmentBodyStatement.Translate
    | Statement.When            (WhenStatement          (_, body))   -> get_inner_optional body WhenBodyStatement.Translate
    | Statement.Unknown         (UnknownStatement    (_, _, body))   -> if body.IsSome then body.Value else []

    | Statement.Module              body ->
        let version, ns, prefix, unknown   = body.Header
        let linkage                        = body.Linkage   |> List.map LinkageBodyStatement.Translate
        let meta                           = body.Meta      |> List.map MetaBodyStatement.Translate
        let revision                       = body.Revision  |> List.map Statement.Revision
        let body'                          = body.Body      |> List.map BodyStatement.Translate

        let unknown' = if unknown.IsSome then unknown.Value |> List.map Statement.Unknown else []

        (Statement.YangVersion version) :: (Statement.Namespace ns) :: (Statement.Prefix prefix) :: unknown' @ linkage @ meta @ revision @ body'

    | Statement.Type                (TypeStatement (_, extra)) ->
        // TODO: Do we need to surface the type restrictions in the list of children of a node?
        if extra.IsSome then TypeBodyStatement.Translate extra.Value else []

    | Statement.Submodule           body ->
        let version, belongs_to, unknown    = body.Header
        let linkage                         = body.Linkage   |> List.map LinkageBodyStatement.Translate
        let meta                            = body.Meta      |> List.map MetaBodyStatement.Translate
        let revision                        = body.Revision  |> List.map Statement.Revision
        let body'                           = body.Body      |> List.map BodyStatement.Translate

        let unknown' = if unknown.IsSome then unknown.Value |> List.map Statement.Unknown else []

        (Statement.YangVersion version) :: (Statement.BelongsTo belongs_to) :: unknown' @ linkage @ meta @ revision @ body'

    let GetIdentifier : Statement -> Identifier option = function
    | Statement.Action              (ActionStatement    (id, _))
    | Statement.AnyData             (AnyDataStatement   (id, _))
    | Statement.AnyXml              (AnyXmlStatement    (id, _))
    | Statement.Argument            (ArgumentStatement  (id, _))
    | Statement.BelongsTo           (BelongsToStatement (id, _))
    | Statement.Bit                 (BitStatement       (id, _))
    | Statement.Case                (CaseStatement      (id, _))
    | Statement.Choice              (ChoiceStatement    (id, _))
    | Statement.Container           (ContainerStatement (id, _))
    | Statement.Extension           (ExtensionStatement (id, _))
    | Statement.Feature             (FeatureStatement   (id, _))
    | Statement.Grouping            (GroupingStatement  (id, _))
    | Statement.Identity            (IdentityStatement  (id, _))
    | Statement.Import              (ImportStatement    (id, _))
    | Statement.Include             (IncludeStatement   (id, _))
    | Statement.Leaf                (LeafStatement      (id, _))
    | Statement.LeafList            (LeafListStatement  (id, _))
    | Statement.List                (ListStatement      (id, _))
    | Statement.Notification        (NotificationStatement (id, _))
    | Statement.Rpc                 (RpcStatement       (id, _))
    | Statement.TypeDef             (TypeDefStatement   (id, _))
        -> Some id

    | Statement.Module              m
        -> Some m.Name
    | Statement.Submodule           m
        -> Some m.Name

    | Statement.Base                _
    | Statement.Type                _
    | Statement.Uses                _
    | Statement.Unknown             _
        -> None

    | Statement.Augment             _
    | Statement.Config              _
    | Statement.Contact             _
    | Statement.Default             _
    | Statement.Description         _
    | Statement.DeviateAdd          _
    | Statement.DeviateDelete       _
    | Statement.DeviateNotSupported _
    | Statement.DeviateReplace      _
    | Statement.Deviation           _
    | Statement.Enum                _
    | Statement.ErrorAppTag         _
    | Statement.ErrorMessage        _
    | Statement.FractionDigits      _
    | Statement.IfFeature           _
    | Statement.Input               _
    | Statement.Key                 _
    | Statement.Length              _
    | Statement.Mandatory           _
    | Statement.MaxElements         _
    | Statement.MinElements         _
    | Statement.Modifier            _
    | Statement.Must                _
    | Statement.Namespace           _
    | Statement.OrderedBy           _
    | Statement.Organization        _
    | Statement.Output              _
    | Statement.Path                _
    | Statement.Pattern             _
    | Statement.Position            _
    | Statement.Prefix              _
    | Statement.Presence            _
    | Statement.Range               _
    | Statement.Reference           _
    | Statement.Refine              _
    | Statement.Revision            _
    | Statement.RevisionDate        _
    | Statement.RequireInstance     _
    | Statement.Status              _
    | Statement.Unique              _
    | Statement.Units               _
    | Statement.UsesAugment         _
    | Statement.When                _
    | Statement.Value               _
    | Statement.YangVersion         _
    | Statement.YinElement          _
        -> None

    let GetReferenceIdentifier : Statement -> IdentifierReference option = function
    | Statement.Action              (ActionStatement        (id, _))
    | Statement.AnyData             (AnyDataStatement       (id, _))
    | Statement.AnyXml              (AnyXmlStatement        (id, _))
    | Statement.Argument            (ArgumentStatement      (id, _))
    | Statement.BelongsTo           (BelongsToStatement     (id, _))
    | Statement.Bit                 (BitStatement           (id, _))
    | Statement.Case                (CaseStatement          (id, _))
    | Statement.Choice              (ChoiceStatement        (id, _))
    | Statement.Container           (ContainerStatement     (id, _))
    | Statement.Extension           (ExtensionStatement     (id, _))
    | Statement.Feature             (FeatureStatement       (id, _))
    | Statement.Grouping            (GroupingStatement      (id, _))
    | Statement.Identity            (IdentityStatement      (id, _))
    | Statement.Import              (ImportStatement        (id, _))
    | Statement.Include             (IncludeStatement       (id, _))
    | Statement.Leaf                (LeafStatement          (id, _))
    | Statement.LeafList            (LeafListStatement      (id, _))
    | Statement.List                (ListStatement          (id, _))
    | Statement.Notification        (NotificationStatement  (id, _))
    | Statement.Rpc                 (RpcStatement           (id, _))
    | Statement.TypeDef             (TypeDefStatement       (id, _))
        -> Some (IdentifierReference.Simple id)

    | Statement.Module              m
        -> Some (IdentifierReference.Simple m.Name)
    | Statement.Submodule           m
        -> Some (IdentifierReference.Simple m.Name)

    | Statement.Base                (BaseStatement (id, _))
    | Statement.Type                (TypeStatement (id, _))
    | Statement.Uses                (UsesStatement (id, _))
        -> Some id

    | Statement.Unknown             (UnknownStatement (id, _, _))
        -> Some (IdentifierReference.Custom id)

    | Statement.Augment             _
    | Statement.Config              _
    | Statement.Contact             _
    | Statement.Default             _
    | Statement.Description         _
    | Statement.DeviateAdd          _
    | Statement.DeviateDelete       _
    | Statement.DeviateNotSupported _
    | Statement.DeviateReplace      _
    | Statement.Deviation           _
    | Statement.Enum                _
    | Statement.ErrorAppTag         _
    | Statement.ErrorMessage        _
    | Statement.FractionDigits      _
    | Statement.IfFeature           _
    | Statement.Input               _
    | Statement.Key                 _
    | Statement.Length              _
    | Statement.Mandatory           _
    | Statement.MaxElements         _
    | Statement.MinElements         _
    | Statement.Modifier            _
    | Statement.Must                _
    | Statement.Namespace           _
    | Statement.OrderedBy           _
    | Statement.Organization        _
    | Statement.Output              _
    | Statement.Path                _
    | Statement.Pattern             _
    | Statement.Position            _
    | Statement.Prefix              _
    | Statement.Presence            _
    | Statement.Range               _
    | Statement.Reference           _
    | Statement.Refine              _
    | Statement.Revision            _
    | Statement.RevisionDate        _
    | Statement.RequireInstance     _
    | Statement.Status              _
    | Statement.Unique              _
    | Statement.Units               _
    | Statement.UsesAugment         _
    | Statement.When                _
    | Statement.Value               _
    | Statement.YangVersion         _
    | Statement.YinElement          _
        -> None

    /// Patterns for the various YANG commands and statement groups
    module Patterns =
        let (|Container|_|) = function
        | Statements.Container v    -> Some v
        | _                         -> None

        let (|Description|_|) = function
        | Statements.Description v  -> Some v
        | _                         -> None

        let (|Grouping|_|) = function
        | Statements.Grouping v     -> Some v
        | _                         -> None

        let (|Leaf|_|) = function
        | Statements.Leaf v         -> Some v
        | _                         -> None

        let (|LeafList|_|) = function
        | Statements.LeafList v     -> Some v
        | _                         -> None

        let (|List|_|) = function
        | Statements.List v         -> Some v
        | _                         -> None

        let (|Prefix|_|) = function
        | Statements.Prefix v       -> Some v
        | _                         -> None

        let (|Type|_|) = function
        | Statements.Type v         -> Some v
        | _                         -> None

        let (|TypeDef|_|) = function
        | Statements.TypeDef v      -> Some v
        | _                         -> None

        let (|Uses|_|) = function
        | Statements.Uses v         -> Some v
        | _                         -> None

        let (|YangVersion|_|) = function
        | Statements.YangVersion v  -> Some v
        | _                         -> None

    /// <summary>
    /// Counts the number of statements that can be
    /// reached from a root statement
    /// </summary>
    /// <param name="statement">The root statement</param>
    let rec CountDescendants (statement : Statement) =
        let children = Children statement

        // TODO: Rewrite as a proper tail recursive function, assuming that it still works on big models
        children
        |> List.fold (
            fun state child ->
                state + (CountDescendants child)
        ) (List.length children)

    let GetDescription (body : Statement list) : string option =
        // Find all description statements.
        // Does the standard disallow multiple description statements
        let descriptions = body |> List.choose Patterns.``|Description|_|``
        if descriptions.Length = 0 then None
        else
            descriptions
            |> Seq.map (fun (DescriptionStatement (description, _)) -> description)
            |> String.concat "\n"
            |> Some
