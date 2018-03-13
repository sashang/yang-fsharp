// StatementHelper.fs
// Helper methods for reasoning about statements
namespace Yang.Model

module StatementHelper =

    let private counter (update : 'T -> int) : 'T list -> int =
        List.fold (fun s v -> s + (update v)) 0

    let private ocounter (update : 'T -> int) (input : 'T list option) =
        if input.IsNone then 0
        else counter (fun v -> 1 + (update v)) (input.Value)

    let private otcounter (transform : 'T -> Statement) (update : Statement -> int) (input : 'T list option) =
        if input.IsNone then 0
        else counter (fun v -> 1 + (update (transform v))) (input.Value)

    let private tcounter (transform : 'T -> Statement) (update : Statement -> int) (input : 'T list) =
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
        -> ocounter Descendants options

    // The following have custom options; the caller need to treat them specially
    | Statement.Action              (_, body) ->
        otcounter ActionBodyStatement.Translate Descendants body
    | Statement.AnyData             (_, body) ->
        otcounter AnyDataBodyStatement.Translate Descendants body
    | Statement.AnyXml              (_, body) ->
        otcounter AnyXmlBodyStatement.Translate Descendants body
    | Statement.Augment             (_, body) ->
        tcounter AugmentBodyStatement.Translate Descendants body
    | Statement.Argument            (_, body) ->
        otcounter ArgumentBodyStatement.Translate Descendants body
    | Statement.BelongsTo           (_, body) ->
        tcounter BelongsToBodyStatement.Translate Descendants body
    | Statement.Bit                 (_, body) ->
        otcounter BitBodyStatement.Translate Descendants body
    | Statement.Case                (_, body) ->
        otcounter CaseBodyStatement.Translate Descendants body
    | Statement.Choice              (_, body) ->
        otcounter ChoiceBodyStatement.Translate Descendants body
    | Statement.Container           (_, body) ->
        otcounter ContainerBodyStatement.Translate Descendants body
    | Statement.DeviateAdd          body ->
        otcounter DeviateAddBodyStatement.Translate Descendants body
    | Statement.DeviateDelete       body ->
        otcounter DeviateDeleteBodyStatement.Translate Descendants body
    | Statement.DeviateReplace      body ->
        otcounter DeviateReplaceBodyStatement.Translate Descendants body
    | Statement.Deviation           (_, body) ->
        otcounter DeviationBodyStatement.Translate Descendants body
    | Statement.Enum                (_, body) ->
        otcounter EnumBodyStatement.Translate Descendants body
    | Statement.Extension           (_, body) ->
        otcounter ExtensionBodyStatement.Translate Descendants body
    | Statement.Feature             (_, body) ->
        otcounter FeatureBodyStatement.Translate Descendants body
    | Statement.Grouping            (_, body) ->
        otcounter GroupingBodyStatement.Translate Descendants body
    | Statement.Identity            (_, body) ->
        otcounter IdentityBodyStatement.Translate Descendants body
    | Statement.Import              (_, body) ->
        tcounter ImportBodyStatement.Translate Descendants body
    | Statement.Include             (_, body) ->
        otcounter IncludeBodyStatement.Translate Descendants body
    | Statement.Input               body ->
        tcounter InputBodyStatement.Translate Descendants body
    | Statement.Leaf                (_, body) ->
        tcounter LeafBodyStatement.Translate Descendants body
    | Statement.LeafList            (_, body) ->
        tcounter LeafListBodyStatement.Translate Descendants body
    | Statement.Length              (_, body) ->
        otcounter LengthBodyStatement.Translate Descendants body
    | Statement.List                (_, body) ->
        tcounter ListBodyStatement.Translate Descendants body
    | Statement.Module              body ->
        // TODO: counter number of statements in module
        10
    | Statement.Must                (_, body) ->
        otcounter MustBodyStatement.Translate Descendants body
    | Statement.Notification        (_, body) ->
        otcounter NotificationBodyStatement.Translate Descendants body
    | Statement.Output              body ->
        tcounter OutputBodyStatement.Translate Descendants body
    | Statement.Pattern             (_, body) ->
        otcounter PatternBodyStatement.Translate Descendants body
    | Statement.Range               (_, body) ->
        otcounter RangeBodyStatement.Translate Descendants body
    | Statement.Rpc                 (_, body) ->
        otcounter RpcBodyStatement.Translate Descendants body
    | Statement.Refine              (_, body) ->
        otcounter RefineBodyStatement.Translate Descendants body
    | Statement.Revision            (_, body) ->
        otcounter RevisionBodyStatement.Translate Descendants body
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
        tcounter TypeDefBodyStatement.Translate Descendants body
    | Statement.Uses                (_, body) ->
        otcounter UsesBodyStatement.Translate Descendants body
    | Statement.UsesAugment         (_, body) ->
        tcounter UsesAugmentBodyStatement.Translate Descendants body
    | Statement.When                (_, body) ->
        otcounter WhenBodyStatement.Translate Descendants body

    | Statement.Unknown body ->
        let _, _, _extra = body
        // TODO: fix counting descendand notes from unknown
        10

    | Statement.Unparsed _ ->
        // TODO: Remove this statement that handles unparsed statements
        failwith "Should not have reached here"


