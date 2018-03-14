// StatementHelper.fs
// Helper methods for reasoning about statements
namespace Yang.Model

module StatementHelper =

    let private get_inner_optional (statements : 'T list option) (map : 'T -> Statement) : Statement list =
        if statements.IsSome then statements.Value |> List.map map
        else []

    let private get_inner (statements : 'T list) (map : 'T -> Statement) : Statement list =
        statements |> List.map map

    let GetIdentifier : Statement -> Identifier option = function
    | Statement.Action              (id, _)
    | Statement.AnyData             (id, _)
    | Statement.AnyXml              (id, _)
    | Statement.Argument            (id, _)
    | Statement.BelongsTo           (id, _)
    | Statement.Bit                 (id, _)
    | Statement.Case                (id, _)
    | Statement.Choice              (id, _)
    | Statement.Container           (id, _)
    | Statement.Extension           (id, _)
    | Statement.Feature             (id, _)
    | Statement.Grouping            (id, _)
    | Statement.Identity            (id, _)
    | Statement.Import              (id, _)
    | Statement.Include             (id, _)
    | Statement.Leaf                (id, _)
    | Statement.LeafList            (id, _)
    | Statement.List                (id, _)
    | Statement.Notification        (id, _)
    | Statement.Rpc                 (id, _)
    | Statement.TypeDef             (id, _)
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

    let Children : Statement -> Statement list = function
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
        -> if options.IsSome then options.Value else []

    | Statement.Action              (_, body)   -> get_inner_optional body ActionBodyStatement.Translate
    | Statement.AnyData             (_, body)   -> get_inner_optional body AnyDataBodyStatement.Translate
    | Statement.AnyXml              (_, body)   -> get_inner_optional body AnyXmlBodyStatement.Translate
    | Statement.Augment             (_, body)   -> get_inner          body AugmentBodyStatement.Translate
    | Statement.Argument            (_, body)   -> get_inner_optional body ArgumentBodyStatement.Translate
    | Statement.BelongsTo           (_, body)   -> get_inner          body BelongsToBodyStatement.Translate
    | Statement.Bit                 (_, body)   -> get_inner_optional body BitBodyStatement.Translate
    | Statement.Case                (_, body)   -> get_inner_optional body CaseBodyStatement.Translate
    | Statement.Choice              (_, body)   -> get_inner_optional body ChoiceBodyStatement.Translate
    | Statement.Container           (_, body)   -> get_inner_optional body ContainerBodyStatement.Translate
    | Statement.DeviateAdd          body        -> get_inner_optional body DeviateAddBodyStatement.Translate
    | Statement.DeviateDelete       body        -> get_inner_optional body DeviateDeleteBodyStatement.Translate
    | Statement.DeviateReplace      body        -> get_inner_optional body DeviateReplaceBodyStatement.Translate
    | Statement.Deviation           (_, body)   -> get_inner_optional body DeviationBodyStatement.Translate
    | Statement.Enum                (_, body)   -> get_inner_optional body EnumBodyStatement.Translate
    | Statement.Extension           (_, body)   -> get_inner_optional body ExtensionBodyStatement.Translate
    | Statement.Feature             (_, body)   -> get_inner_optional body FeatureBodyStatement.Translate
    | Statement.Grouping            (_, body)   -> get_inner_optional body GroupingBodyStatement.Translate
    | Statement.Identity            (_, body)   -> get_inner_optional body IdentityBodyStatement.Translate
    | Statement.Import              (_, body)   -> get_inner          body ImportBodyStatement.Translate
    | Statement.Include             (_, body)   -> get_inner_optional body IncludeBodyStatement.Translate
    | Statement.Input               body        -> get_inner          body InputBodyStatement.Translate
    | Statement.Leaf                (_, body)   -> get_inner          body LeafBodyStatement.Translate
    | Statement.LeafList            (_, body)   -> get_inner          body LeafListBodyStatement.Translate
    | Statement.Length              (_, body)   -> get_inner_optional body LengthBodyStatement.Translate
    | Statement.List                (_, body)   -> get_inner          body ListBodyStatement.Translate
    | Statement.Must                (_, body)   -> get_inner_optional body MustBodyStatement.Translate
    | Statement.Notification        (_, body)   -> get_inner_optional body NotificationBodyStatement.Translate
    | Statement.Output              body        -> get_inner          body OutputBodyStatement.Translate
    | Statement.Pattern             (_, body)   -> get_inner_optional body PatternBodyStatement.Translate
    | Statement.Range               (_, body)   -> get_inner_optional body RangeBodyStatement.Translate
    | Statement.Rpc                 (_, body)   -> get_inner_optional body RpcBodyStatement.Translate
    | Statement.Refine              (_, body)   -> get_inner_optional body RefineBodyStatement.Translate
    | Statement.Revision            (_, body)   -> get_inner_optional body RevisionBodyStatement.Translate
    | Statement.TypeDef             (_, body)   -> get_inner          body TypeDefBodyStatement.Translate
    | Statement.Uses                (_, body)   -> get_inner_optional body UsesBodyStatement.Translate
    | Statement.UsesAugment         (_, body)   -> get_inner          body UsesAugmentBodyStatement.Translate
    | Statement.When                (_, body)   -> get_inner_optional body WhenBodyStatement.Translate
    | Statement.Unknown          (_, _, body)   -> if body.IsSome then body.Value else []

    | Statement.Module              body ->
        let version, ns, prefix, unknown   = body.Header
        let linkage                        = body.Linkage   |> List.map LinkageBodyStatement.Translate
        let meta                           = body.Meta      |> List.map MetaBodyStatement.Translate
        let revision                       = body.Revision  |> List.map Statement.Revision
        let body'                          = body.Body      |> List.map BodyStatement.Translate

        let unknown' = if unknown.IsSome then unknown.Value |> List.map Statement.Unknown else []

        (Statement.YangVersion version) :: (Statement.Namespace ns) :: (Statement.Prefix prefix) :: unknown' @ linkage @ meta @ revision @ body'

    | Statement.Type                (_, _, extra) ->
        // TODO: Do we need to surface the type restrictions in the list of children of a node?
        if extra.IsSome then extra.Value |> List.map Statement.Unknown else []

    | Statement.Submodule           body ->
        let version, belongs_to, unknown    = body.Header
        let linkage                         = body.Linkage   |> List.map LinkageBodyStatement.Translate
        let meta                            = body.Meta      |> List.map MetaBodyStatement.Translate
        let revision                        = body.Revision  |> List.map Statement.Revision
        let body'                           = body.Body      |> List.map BodyStatement.Translate

        let unknown' = if unknown.IsSome then unknown.Value |> List.map Statement.Unknown else []

        (Statement.YangVersion version) :: (Statement.BelongsTo belongs_to) :: unknown' @ linkage @ meta @ revision @ body'

    module Patterns =
        let (|Container|_|) = function
        | Statements.Container v    -> Some v
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

        let (|Type|_|) = function
        | Statements.Type v         -> Some v
        | _                         -> None

        let (|TypeDef|_|) = function
        | Statements.TypeDef v      -> Some v
        | _                         -> None

        let (|Uses|_|) = function
        | Statements.Uses v         -> Some v
        | _                         -> None


    let rec CountDescendants (statement : Statement) =
        let children = Children statement
        let starting =
            match statement with
            | Patterns.Type (_, restriction, extra) ->
                if restriction.IsSome then 1 else 0
            | _ -> 0

        // TODO: Rewrite as a proper tail recursive function, assuming that it still works on big models
        children
        |> List.fold (
            fun state child ->
                state + (CountDescendants child)
        ) (starting + List.length children)

