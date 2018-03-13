// StatementHelper.fs
// Helper methods for reasoning about statements
namespace Yang.Model

module StatementHelper =

    let private counter (update : 'T -> int) : 'T list -> int =
        List.fold (fun s v -> s + (update v)) 0

    let private counter_o (update : 'T -> int) (input : 'T list option) =
        if input.IsNone then 0
        else counter (fun v -> 1 + (update v)) (input.Value)

    let private counter_ot (transform : 'T -> Statement) (update : Statement -> int) (input : 'T list option) =
        if input.IsNone then 0
        else counter (fun v -> 1 + (update (transform v))) (input.Value)

    let private counter_t (transform : 'T -> Statement) (update : Statement -> int) (input : 'T list) =
        counter (fun v -> 1 + (update (transform v))) input

    /// <summary>
    /// Computes the number of descendant nodes from 
    /// </summary>
    let rec Descendants = function
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
        -> counter_o Descendants options

    // The following have custom options; the caller need to treat them specially
    | Statement.Action              (_, body) ->
        counter_ot ActionBodyStatement.Translate Descendants body
    | Statement.AnyData             (_, body) ->
        counter_ot AnyDataBodyStatement.Translate Descendants body
    | Statement.AnyXml              (_, body) ->
        counter_ot AnyXmlBodyStatement.Translate Descendants body
    | Statement.Augment             (_, body) ->
        counter_t AugmentBodyStatement.Translate Descendants body
    | Statement.Argument            (_, body) ->
        counter_ot ArgumentBodyStatement.Translate Descendants body
    | Statement.BelongsTo           (_, body) ->
        counter_t BelongsToBodyStatement.Translate Descendants body
    | Statement.Bit                 (_, body) ->
        counter_ot BitBodyStatement.Translate Descendants body
    | Statement.Case                (_, body) ->
        counter_ot CaseBodyStatement.Translate Descendants body
    | Statement.Choice              (_, body) ->
        counter_ot ChoiceBodyStatement.Translate Descendants body
    | Statement.Container           (_, body) ->
        counter_ot ContainerBodyStatement.Translate Descendants body
    | Statement.DeviateAdd          body ->
        counter_ot DeviateAddBodyStatement.Translate Descendants body
    | Statement.DeviateDelete       body ->
        counter_ot DeviateDeleteBodyStatement.Translate Descendants body
    | Statement.DeviateReplace      body ->
        counter_ot DeviateReplaceBodyStatement.Translate Descendants body
    | Statement.Deviation           (_, body) ->
        counter_ot DeviationBodyStatement.Translate Descendants body
    | Statement.Enum                (_, body) ->
        counter_ot EnumBodyStatement.Translate Descendants body
    | Statement.Extension           (_, body) ->
        counter_ot ExtensionBodyStatement.Translate Descendants body
    | Statement.Feature             (_, body) ->
        counter_ot FeatureBodyStatement.Translate Descendants body
    | Statement.Grouping            (_, body) ->
        counter_ot GroupingBodyStatement.Translate Descendants body
    | Statement.Identity            (_, body) ->
        counter_ot IdentityBodyStatement.Translate Descendants body
    | Statement.Import              (_, body) ->
        counter_t ImportBodyStatement.Translate Descendants body
    | Statement.Include             (_, body) ->
        counter_ot IncludeBodyStatement.Translate Descendants body
    | Statement.Input               body ->
        counter_t InputBodyStatement.Translate Descendants body
    | Statement.Leaf                (_, body) ->
        counter_t LeafBodyStatement.Translate Descendants body
    | Statement.LeafList            (_, body) ->
        counter_t LeafListBodyStatement.Translate Descendants body
    | Statement.Length              (_, body) ->
        counter_ot LengthBodyStatement.Translate Descendants body
    | Statement.List                (_, body) ->
        counter_t ListBodyStatement.Translate Descendants body
    | Statement.Module              body ->
        // TODO: counter number of statements in module
        10
    | Statement.Must                (_, body) ->
        counter_ot MustBodyStatement.Translate Descendants body
    | Statement.Notification        (_, body) ->
        counter_ot NotificationBodyStatement.Translate Descendants body
    | Statement.Output              body ->
        counter_t OutputBodyStatement.Translate Descendants body
    | Statement.Pattern             (_, body) ->
        counter_ot PatternBodyStatement.Translate Descendants body
    | Statement.Range               (_, body) ->
        counter_ot RangeBodyStatement.Translate Descendants body
    | Statement.Rpc                 (_, body) ->
        counter_ot RpcBodyStatement.Translate Descendants body
    | Statement.Refine              (_, body) ->
        counter_ot RefineBodyStatement.Translate Descendants body
    | Statement.Revision            (_, body) ->
        counter_ot RevisionBodyStatement.Translate Descendants body
    | Statement.Submodule           body ->
        // TODO: counter number of statements in sub-module
        10
    | Statement.Type                (_, restrictions, extra) ->
        let e =
            if extra.IsNone then 0
            else
                extra.Value
                |> List.map UnknownStatement.Translate
                |> List.fold (fun total v -> total + (Descendants v) + 1) 0
        // TODO: count number of statements in restrictions
        let r = 0
        e + r
    | Statement.TypeDef             (_, body) ->
        counter_t TypeDefBodyStatement.Translate Descendants body
    | Statement.Uses                (_, body) ->
        counter_ot UsesBodyStatement.Translate Descendants body
    | Statement.UsesAugment         (_, body) ->
        counter_t UsesAugmentBodyStatement.Translate Descendants body
    | Statement.When                (_, body) ->
        counter_ot WhenBodyStatement.Translate Descendants body

    | Statement.Unknown body ->
        let _, _, _extra = body
        // TODO: fix counting descendand notes from unknown
        10

    | Statement.Unparsed _ ->
        // TODO: Remove this statement that handles unparsed statements
        failwith "Should not have reached here"


