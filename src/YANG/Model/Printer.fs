// Printer.fs
// Implements model printing functionality
namespace Yang.Model

module Printer =
    open System.Text

    // TODO: Implement a compact pretty printer

    type YangPrinter (?sb : StringBuilder, ?indentation : int, ?indent : string) as this =
        let sb = defaultArg sb (System.Text.StringBuilder ())
        let indent = defaultArg indent "\t"
        let compact = false
        let mutable indentation = defaultArg indentation 0

        let indent ()   = Printf.bprintf sb "%s" (String.replicate indentation "    ")

        let push () = indentation <- indentation + 1
        let pop  () = indentation <- indentation - 1

        let bb () = Printf.bprintf sb " {\n"; push ()
        let eb () = pop(); indent(); Printf.bprintf sb "}\n"

        let oblock (body : 'T list option) (f : 'T -> unit) =
            if body.IsNone then Printf.bprintf sb ";\n"
            elif body.Value.Length = 0 then Printf.bprintf sb " {}\n"
            elif body.Value.Length = 1 && compact then
                Printf.bprintf sb " { "
                f (body.Value.Head)
                Printf.bprintf sb " }\n"
            else
                bb ()
                body.Value |> List.iter f
                eb ()

        let block (body : 'T list) (f : 'T -> unit) =
            if body.Length = 0 then Printf.bprintf sb " {}\n"
            elif body.Length = 1 && compact then
                Printf.bprintf sb " { "
                f (body.Head)
                Printf.bprintf sb " }\n"
            else
                bb ()
                body |> List.iter f
                eb ()

        member __.Append (statement : ActionStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "action %s" id.Value
            oblock body (fun s -> this.Append (ActionBodyStatement.Translate s))

        member __.Append (statement : AnyDataStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "anydata %s" id.Value
            oblock body (fun s -> this.Append (AnyDataBodyStatement.Translate s))

        member __.Append (statement : AnyXmlStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "anyxml %s" id.Value
            oblock body (fun s -> this.Append (AnyXmlBodyStatement.Translate s))

        member __.Append (statement : ArgumentStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "argument %s" id.Value
            oblock body (fun s -> this.Append (ArgumentBodyStatement.Translate s))

        member __.Append (statement : AugmentStatement) =
            let augment, body = statement
            indent (); Printf.bprintf sb "augment %s" augment.Value
            block body (fun s -> this.Append (AugmentBodyStatement.Translate s))

        member __.Append (statement : BelongsToStatement) =
            let augment, body = statement
            indent (); Printf.bprintf sb "belongs-to %s" augment.Value
            block body (fun s -> this.Append (BelongsToBodyStatement.Translate s))

        member __.Append (statement : BaseStatement) =
            let augment, extra = statement
            indent (); Printf.bprintf sb "base %s" augment.Value
            this.Append extra

        member __.Append (statement : BitStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "bit %s" id.Value
            oblock body (fun s -> this.Append (BitBodyStatement.Translate s))

        member __.Append (statement : BodyStatement) =
            match statement with
            | BodyStatement.AnyData     st  -> this.Append st
            | BodyStatement.AnyXml      st  -> this.Append st
            | BodyStatement.Augment     st  -> this.Append st
            | BodyStatement.Choice      st  -> this.Append st
            | BodyStatement.Container   st  -> this.Append st
            | BodyStatement.Deviation   st  -> this.Append st
            | BodyStatement.Extension   st  -> this.Append st
            | BodyStatement.Feature     st  -> this.Append st
            | BodyStatement.Grouping    st  -> this.Append st
            | BodyStatement.Identity    st  -> this.Append st
            | BodyStatement.Leaf        st  -> this.Append st
            | BodyStatement.LeafList    st  -> this.Append st
            | BodyStatement.List        st  -> this.Append st
            | BodyStatement.Notification st -> this.Append st
            | BodyStatement.Rpc         st  -> this.Append st
            | BodyStatement.TypeDef     st  -> this.Append st
            | BodyStatement.Uses        st  -> this.Append st
            | BodyStatement.Unknown     st  -> this.Append st

        member __.Append (statement : CaseStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "bit %s" id.Value
            oblock body (fun s -> this.Append (CaseBodyStatement.Translate s))

        member __.Append (statement : ChoiceStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "choice %s" id.Value
            oblock body (fun s -> this.Append (ChoiceBodyStatement.Translate s))

        member __.Append (statement : ConfigStatement) =
            let b, extra = statement
            indent (); Printf.bprintf sb "ocnfig %s" (if b then "true" else "false")
            this.Append extra

        member __.Append (statement : ContainerStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "container %s" id.Value
            oblock body (fun s -> this.Append (ContainerBodyStatement.Translate s))

        member __.Append (body : DeviateAddStatement) =
            indent(); Printf.bprintf sb "deviate add"
            oblock body (fun s -> this.Append (DeviateAddBodyStatement.Translate s))

        member __.Append (body : DeviateDeleteStatement) =
            indent(); Printf.bprintf sb "deviate delete"
            oblock body (fun s -> this.Append (DeviateDeleteBodyStatement.Translate s))

        member __.Append (body : DeviateReplaceStatement) =
            indent(); Printf.bprintf sb "deviate replace"
            oblock body (fun s -> this.Append (DeviateReplaceBodyStatement.Translate s))

        member __.Append (statement : DeviationStatement) =
            let deviation, body = statement
            indent(); Printf.bprintf sb "deviation %s" deviation.Value
            oblock body (fun s -> this.Append (DeviationBodyStatement.Translate s))

        member __.Append (statement : EnumStatement) =
            let enum, body = statement
            indent (); Printf.bprintf sb "enum %s" enum
            oblock body (fun s -> this.Append (EnumBodyStatement.Translate s))

        member __.Append (statement : ExtensionStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "extension %s" id.Value
            oblock body (fun s -> this.Append (ExtensionBodyStatement.Translate s))

        member __.Append (statement : FeatureStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "feature %s" id.Value
            oblock body (fun s -> this.Append (FeatureBodyStatement.Translate s))

        member __.Append (statement : FractionDigitsStatement) =
            let fraction, extra = statement
            indent(); Printf.bprintf sb "fraction-digits %d" fraction
            this.Append extra

        member __.Append (statement : GroupingStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "grouping %s" id.Value
            oblock body (fun s -> this.Append (GroupingBodyStatement.Translate s))

        member __.Append (statement : IdentityStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "identity %s" id.Value
            oblock body (fun s -> this.Append (IdentityBodyStatement.Translate s))

        member __.Append (statement : IfFeatureStatement) =
            let expr, extra = statement
            indent(); Printf.bprintf sb "if-feature "; (Expressions.PrettyPrint (sb, indentation) expr)
            this.Append extra

        member __.Append (statement : ImportBodyStatement) =
             match statement with
             | ImportBodyStatement.Description st   -> this.Append st
             | ImportBodyStatement.Prefix st        -> this.Append st
             | ImportBodyStatement.Reference st     -> this.Append st
             | ImportBodyStatement.RevisionDate st  -> this.Append st
             | ImportBodyStatement.Unknown st       -> this.Append st

        member __.Append (statement : ImportStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "import %s" id.Value
            block body (fun s -> this.Append (ImportBodyStatement.Translate s))

        member __.Append (statement : IncludeBodyStatement) =
            match statement with
            | IncludeBodyStatement.Description  st  -> this.Append st
            | IncludeBodyStatement.Reference    st  -> this.Append st
            | IncludeBodyStatement.RevisionDate st  -> this.Append st
            | IncludeBodyStatement.Unknown      st  -> this.Append st

        member __.Append (statement : IncludeStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "include %s" id.Value
            oblock body (fun s -> this.Append (IncludeBodyStatement.Translate s))

        member __.Append (body : InputStatement) =
            indent(); Printf.bprintf sb "input"
            block body (fun s -> this.Append (InputBodyStatement.Translate s))

        member __.Append (statement : KeyStatement) =
            let key, extra = statement
            indent(); Printf.bprintf sb "key %s" (Arguments.Key.Value key)
            this.Append extra

        member __.Append (statement : LeafStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "leaf %s" id.Value
            block body (fun s -> this.Append (LeafBodyStatement.Translate s))

        member __.Append (statement : LeafListStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "leaf-list %s" id.Value
            block body (fun s -> this.Append (LeafListBodyStatement.Translate s))

        member __.Append (statement : LengthStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "length %s" id.Value
            oblock body (fun s -> this.Append (LengthBodyStatement.Translate s))

        member __.Append (statement : LinkageBodyStatement) =
            match statement with
            | Import statement  -> this.Append statement
            | Include statement -> this.Append statement

        member __.Append (statement : ListStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "list %s" id.Value
            block body (fun s -> this.Append (ListBodyStatement.Translate s))

        member __.Append (statement : MaxElementsStatement) =
            let max, extra = statement
            indent(); Printf.bprintf sb "max-elements %s" max.Value
            this.Append extra

        member __.Append (statement : MetaBodyStatement) =
            match statement with
            | MetaBodyStatement.Contact st      -> this.Append st
            | MetaBodyStatement.Description st  -> this.Append st
            | MetaBodyStatement.Organization st -> this.Append st
            | MetaBodyStatement.Reference   st  -> this.Append st
            | MetaBodyStatement.Unknown     st  -> this.Append st

        member __.Append (statement : MinElementsStatement) =
            let min, extra = statement
            indent(); Printf.bprintf sb "min-elements %s" min.Value
            this.Append extra

        member __.Append (statement : ModuleStatement) =
            indent ()
            Printf.bprintf sb "module %s {\n" statement.Name.Value
            indentation <- indentation + 1

            let version, ns, prefix, unknowns = statement.Header
            this.Append version
            this.Append ns
            this.Append prefix
            if unknowns.IsSome then indent(); this.Append unknowns

            let linkage = statement.Linkage
            if linkage.IsEmpty = false then
                // leave an empty line
                Printf.bprintf sb "\n"
                linkage |> List.iter (this.Append)

            let meta = statement.Meta
            if meta.Length > 0 then
                Printf.bprintf sb "\n"
                meta |> List.iter (this.Append)

            let revision = statement.Revision
            if revision.IsEmpty = false then
                Printf.bprintf sb "\n"
                revision |> List.iter (this.Append)

            let body = statement.Body
            if body.IsEmpty = false then
                Printf.bprintf sb "\n"
                body |> List.iter (this.Append)

            indentation <- indentation - 1
            indent ()
            Printf.bprintf sb "}\n"

        member __.Append (statement : ModifierStatement) =
            let modifier, extra = statement
            indent(); Printf.bprintf sb "modifier %s" modifier.Value
            this.Append extra

        member __.Append (statement : MustStatement) =
            let condition, body = statement
            indent(); Printf.bprintf sb "must %s" condition
            oblock body (fun s -> this.Append (MustBodyStatement.Translate s))

        member __.Append (statement : NamespaceStatement) =
            let uri, extra = statement
            indent ()
            Printf.bprintf sb "namespace \"%s\"" (uri.ToString())
            this.Append extra

        member __.Append (statement : NotificationStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "Notification %s" id.Value
            oblock body (fun s -> this.Append (NotificationBodyStatement.Translate s))

        member __.Append (statement : OrderedByStatement) =
            let order, extra = statement
            indent(); Printf.bprintf sb "must %s" order.Value
            this.Append extra

        member __.Append (body : OutputStatement) =
            indent(); Printf.bprintf sb "output"
            block body (fun s -> this.Append (OutputBodyStatement.Translate s))

        member __.Append (statement : PathStatement) =
            let path, extra = statement
            indent ()
            Printf.bprintf sb "path %s" (Arguments.PathListValue path)
            this.Append extra

        member __.Append (statement : PatternStatement) =
            let pattern, body = statement
            indent(); Printf.bprintf sb "pattern \"%s\"" pattern
            oblock body (fun s -> this.Append (PatternBodyStatement.Translate s))

        member __.Append (statement : PrefixStatement) =
            let prefix, extra = statement
            indent ()
            Printf.bprintf sb "prefix %s" prefix
            this.Append extra

        member __.Append (statement : PositionStatement) =
            let position, extra = statement
            indent(); Printf.bprintf sb "position %d" position
            this.Append extra

        member __.Append (statement : RangeStatement) =
            let range, body = statement
            indent(); Printf.bprintf sb "range %s" range.Value
            oblock body (fun s -> this.Append (RangeBodyStatement.Translate s))

        member __.Append (statement : RefineStatement) =
            let refine, body = statement
            indent(); Printf.bprintf sb "range %s" refine.Value
            oblock body (fun s -> this.Append (RefineBodyStatement.Translate s))

        member __.Append (statement : RevisionBodyStatement) =
            match statement with
            | RevisionBodyStatement.Description st  -> this.Append st
            | RevisionBodyStatement.Reference   st  -> this.Append st
            | RevisionBodyStatement.Unknown     st  -> this.Append st

        member __.Append (statement : RevisionStatement) =
            let date, body = statement
            indent (); Printf.bprintf sb "revision %s" date.Value
            oblock body (fun s -> this.Append (RevisionBodyStatement.Translate s))

        member __.Append (statement : RevisionDateStatement) =
            let date, extra = statement;
            indent ()
            Printf.bprintf sb "revision %s" date.Value
            this.Append extra

        member __.Append (statement : RpcStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "rpc %s" id.Value
            oblock body (fun s -> this.Append (RpcBodyStatement.Translate s))

        member __.Append (statement : StatusStatement) =
            let status, extra = statement
            indent(); Printf.bprintf sb "status %s" status.Value
            this.Append extra

        member __.Append (statement : SubmoduleStatement) =
            indent ()
            Printf.bprintf sb "submodule %s {\n" statement.Name.Value
            indentation <- indentation + 1

            let version, belongsTo, unknowns = statement.Header
            this.Append version
            this.Append belongsTo
            this.Append unknowns

            let linkage = statement.Linkage
            if linkage.IsEmpty = false then
                // leave an empty line
                Printf.bprintf sb "\n"
                linkage |> List.iter (this.Append)

            let meta = statement.Meta
            if meta.Length > 0 then
                Printf.bprintf sb "\n"
                meta |> List.iter (this.Append)

            let revision = statement.Revision
            if revision.IsEmpty = false then
                Printf.bprintf sb "\n"
                revision |> List.iter (this.Append)

            let body = statement.Body
            if body.IsEmpty = false then
                Printf.bprintf sb "\n"
                body |> List.iter (this.Append)

            indentation <- indentation - 1
            indent ()
            Printf.bprintf sb "}\n"


        member __.Append (statement : TypeStatement) =
            let id, arg, body = statement
            indent(); Printf.bprintf sb "type %s" id.Value

            if arg.IsNone && body.IsNone then Printf.bprintf sb ";\n"
            elif arg.IsNone && body.Value.Length = 0 then Printf.bprintf sb " {}\n"
            else
                Printf.bprintf sb "{\n"
                indent ()

                if arg.IsSome then
                    // TODO: Pretty print TypeBodyStatement
                    Printf.bprintf sb " %A\n" arg.Value
                    indent()

                if body.IsSome then
                    body.Value
                    |> List.iter (
                        fun b ->
                            this.Append b
                            indent ()
                    )

        member __.Append (statement : TypeDefStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "typedef %s" id.Value
            block body (fun s -> this.Append (TypeDefBodyStatement.Translate s))

        member __.Append (statement : UniqueStatement) =
            let unique, extra = statement
            indent(); Printf.bprintf sb "unique %s" unique.Value
            this.Append extra

        member __.Append (statement : UsesStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "uses %s" id.Value
            oblock body (fun s -> this.Append (UsesBodyStatement.Translate s))

        member __.Append (statement : UsesAugmentStatement) =
            let augment, body = statement
            indent(); Printf.bprintf sb "augment %s" augment.Value
            block body (fun s -> this.Append (UsesAugmentBodyStatement.Translate s))

        member __.Append (statement : ValueStatement) =
            let value, extra = statement
            indent(); Printf.bprintf sb "value %d" value
            this.Append extra

        member __.Append (statement : WhenStatement) =
            let condition, body = statement
            indent(); Printf.bprintf sb "when %s" condition
            oblock body (fun s -> this.Append (WhenBodyStatement.Translate s))

        member __.Append (statement : YangVersionStatement) =
            let version, extra = statement
            indent ()
            Printf.bprintf sb "yang-version %d.%d" version.Major version.Minor
            this.Append extra

        member __.Append (statement : Statement) =
            match statement with
            | Statement.Action st               -> this.Append st
            | Statement.AnyData st              -> this.Append st
            | Statement.AnyXml st               -> this.Append st
            | Statement.Argument st             -> this.Append st
            | Statement.Augment st              -> this.Append st
            | Statement.Base st                 -> this.Append st
            | Statement.BelongsTo st            -> this.Append st
            | Statement.Bit st                  -> this.Append st
            | Statement.Case st                 -> this.Append st
            | Statement.Choice st               -> this.Append st
            | Statement.Config st               -> this.Append st
            | Statement.Contact st              -> this.Append st
            | Statement.Container st            -> this.Append st
            | Statement.Default st              -> this.Append st
            | Statement.Description st          -> this.Append st
            | Statement.DeviateAdd st           -> this.Append st
            | Statement.DeviateDelete st        -> this.Append st
            | Statement.DeviateNotSupported st  -> this.Append st
            | Statement.DeviateReplace st       -> this.Append st
            | Statement.Deviation st            -> this.Append st
            | Statement.Enum st                 -> this.Append st
            | Statement.ErrorAppTag st          -> this.Append st
            | Statement.ErrorMessage st         -> this.Append st
            | Statement.Extension st            -> this.Append st
            | Statement.Feature st              -> this.Append st
            | Statement.FractionDigits st       -> this.Append st
            | Statement.Grouping st             -> this.Append st
            | Statement.Identity st             -> this.Append st
            | Statement.IfFeature st            -> this.Append st
            | Statement.Import st               -> this.Append st
            | Statement.Include st              -> this.Append st
            | Statement.Input st                -> this.Append st
            | Statement.Key st                  -> this.Append st
            | Statement.Leaf st                 -> this.Append st
            | Statement.LeafList st             -> this.Append st
            | Statement.Length st               -> this.Append st
            | Statement.List st                 -> this.Append st
            | Statement.Mandatory st            -> this.Append st
            | Statement.MaxElements st          -> this.Append st
            | Statement.MinElements st          -> this.Append st
            | Statement.Modifier st             -> this.Append st
            | Statement.Module st               -> this.Append st
            | Statement.Must st                 -> this.Append st
            | Statement.Namespace st            -> this.Append st
            | Statement.Notification st         -> this.Append st
            | Statement.OrderedBy st            -> this.Append st
            | Statement.Organization st         -> this.Append st
            | Statement.Output st               -> this.Append st
            | Statement.Path st                 -> this.Append st
            | Statement.Pattern st              -> this.Append st
            | Statement.Position st             -> this.Append st
            | Statement.Prefix st               -> this.Append st
            | Statement.Presence st             -> this.Append st
            | Statement.Range st                -> this.Append st
            | Statement.Reference st            -> this.Append st
            | Statement.Refine st               -> this.Append st
            | Statement.Revision st             -> this.Append st
            | Statement.RevisionDate st         -> this.Append st
            | Statement.RequireInstance st      -> this.Append st
            | Statement.Rpc st                  -> this.Append st
            | Statement.Status st               -> this.Append st
            | Statement.Submodule st            -> this.Append st
            | Statement.Type st                 -> this.Append st
            | Statement.TypeDef st              -> this.Append st
            | Statement.Unique st               -> this.Append st
            | Statement.Units st                -> this.Append st
            | Statement.Unknown st              -> this.Append st
            | Statement.Uses st                 -> this.Append st
            | Statement.UsesAugment st          -> this.Append st
            | Statement.Value st                -> this.Append st
            | Statement.When st                 -> this.Append st
            | Statement.YangVersion st          -> this.Append st
            | Statement.YinElement st           -> this.Append st
            // TODO: remove the following statement
            | Statement.Unparsed (_,_,_)             -> failwith "should not existed"

        member __.Append (statement : ExtraStatements) =
            match statement with
            | None              -> Printf.bprintf sb ";\n"
            | Some statements   ->
                Printf.bprintf sb " {\n"
                indentation <- indentation + 1

                statements |> List.iter (this.Append)

                indentation <- indentation - 1
                indent ()
                Printf.bprintf sb "}\n"

        member __.Append (statement : UnknownStatement) =
            let id, arg, extra = statement
            Printf.bprintf sb "%s" (id.ToString())
            if arg.IsSome then Printf.bprintf sb " %s" (arg.Value)
            this.Append extra

        member __.Append (unknowns : UnknownStatement list option) =
            match unknowns with
            | None          -> Printf.bprintf sb ";\n"
            | Some unknowns ->
                unknowns |> List.iter (this.Append)

        member __.Clear() =
            sb.Clear() |> ignore
            indentation <- 0

        override this.ToString() = sb.ToString()

    let ModuleToString (statement : ModuleStatement) =
        let printer = YangPrinter ()
        printer.Append statement
        printer.ToString()
