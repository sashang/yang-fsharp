// Printer.fs
// Implements model printing functionality
namespace Yang.Model

open System

module Printer =
    open System.Text

    /// Create a well-formed string in the YANG language.
    let ToYangString (input : string) =
        // The following characters are not allowed in un-escaped strings
        let not_allowed_in_simple_strings = [| ' '; '\t'; '\n'; '\r'; '\\'; '"'; '\'' |]

        if input.IndexOfAny(not_allowed_in_simple_strings) < 0 then input
        elif input.IndexOf('\'') < 0 then
            // The string can be encoded as a single-quoted string
            sprintf "'%s'" input
        else
            // We need to encode the string as a double-quoted string
            let input' = input.Replace(@"\", @"\\")
                              .Replace("\"", "\\\"")

            sprintf "\"%s\"" input'

    /// Create a well-formed string in the YANG language, with quotes.
    let ToYangStringQuoted (input : string) =
        if input.IndexOf('\'') < 0 then
            // The string can be encoded as a single-quoted string
            sprintf "'%s'" input
        else
            // We need to encode the string as a double-quoted string
            let input' = input.Replace(@"\", @"\\")
                              .Replace("\"", "\\\"")

            sprintf "\"%s\"" input'

    /// Internal state of the pretty printer
    type private PrinterState =
    /// Print normally.
    | Normal
    /// The current statement is shallow (has only one child), suppress newlines for its child.
    | NextSuppress
    /// Newlines are suppressed
    | Suppressed
    with
        member this._IsNormal = match this with | Normal -> true | _ -> false
        member this._IsSuppressed = match this with | Suppressed -> true | _ -> false
        member this._IsNotSuppressed = match this with | Suppressed -> false | _ -> true
        member this._IsNextSuppress = match this with | NextSuppress -> true | _ -> false

    type YangPrinter (?sb : StringBuilder, ?indentation : int, ?indent : string, ?compact : bool) as this =
        let sb = defaultArg sb (System.Text.StringBuilder ())
        let indent = defaultArg indent "    "
        let compact = defaultArg compact false
        let mutable indentation = defaultArg indentation 0
        let mutable mode : PrinterState = Normal

        let indent ()   =
            if mode._IsNotSuppressed then Printf.bprintf sb "%s" (String.replicate indentation indent)
            else Printf.bprintf sb " "

        let push () = indentation <- indentation + 1
        let pop  () = indentation <- indentation - 1
        let nl   () = if mode._IsNotSuppressed then Printf.bprintf sb "\n"

        let bb () =
            Printf.bprintf sb " {"
            if mode._IsNormal then nl()
            push ()

        let eb () =
            pop()
            if mode._IsNormal then indent() else Printf.bprintf sb " "
            Printf.bprintf sb "}"
            nl()

        let block_o (body : 'T list option) (f : 'T -> unit) =
            if body.IsNone then Printf.bprintf sb ";"; nl()
            elif body.Value.Length = 0 then Printf.bprintf sb " {}"; nl()
            else
                bb ()
                body.Value |> List.iter f
                eb ()

        let block (body : 'T list) (f : 'T -> unit) =
            if body.Length = 0 then Printf.bprintf sb " {}"; nl()
            else
                bb ()
                body |> List.iter f
                eb ()

        member __.Append (statement : ActionStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "action %s" id.Value
            block_o body (fun s -> this.Append (ActionBodyStatement.Translate s))

        member __.Append (statement : AnyDataStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "anydata %s" id.Value
            block_o body (fun s -> this.Append (AnyDataBodyStatement.Translate s))

        member __.Append (statement : AnyXmlStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "anyxml %s" id.Value
            block_o body (fun s -> this.Append (AnyXmlBodyStatement.Translate s))

        member __.Append (statement : ArgumentStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "argument %s" id.Value
            block_o body (fun s -> this.Append (ArgumentBodyStatement.Translate s))

        member __.Append (statement : AugmentStatement) =
            let augment, body = statement
            indent (); Printf.bprintf sb "augment %s" (ToYangStringQuoted augment.Value)
            block body (fun s -> this.Append (AugmentBodyStatement.Translate s))

        member __.Append (statement : BelongsToStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "belongs-to %s" id.Value
            block body (fun s -> this.Append (BelongsToBodyStatement.Translate s))

        member __.Append (statement : BaseStatement) =
            let id, extra = statement
            indent (); Printf.bprintf sb "base %s" id.Value
            this.Append extra

        member __.Append (statement : BitStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "bit %s" id.Value
            block_o body (fun s -> this.Append (BitBodyStatement.Translate s))

        member __.Append (statement : BodyStatement) =
            this.Append (BodyStatement.Translate statement)

        member __.Append (statement : CaseStatement) =
            let id, body = statement
            indent (); Printf.bprintf sb "case %s" id.Value
            block_o body (fun s -> this.Append (CaseBodyStatement.Translate s))

        member __.Append (statement : ChoiceStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "choice %s" id.Value
            block_o body (fun s -> this.Append (ChoiceBodyStatement.Translate s))

        member __.Append (statement : ConfigStatement) =
            let b, extra = statement
            indent (); Printf.bprintf sb "config %s" (if b then "true" else "false")
            this.Append extra

        member __.Append (statement : ContainerStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "container %s" id.Value
            block_o body (fun s -> this.Append (ContainerBodyStatement.Translate s))

        member __.Append (body : DeviateAddStatement) =
            indent(); Printf.bprintf sb "deviate add"
            block_o body (fun s -> this.Append (DeviateAddBodyStatement.Translate s))

        member __.Append (body : DeviateDeleteStatement) =
            indent(); Printf.bprintf sb "deviate delete"
            block_o body (fun s -> this.Append (DeviateDeleteBodyStatement.Translate s))

        member __.Append (body : DeviateReplaceStatement) =
            indent(); Printf.bprintf sb "deviate replace"
            block_o body (fun s -> this.Append (DeviateReplaceBodyStatement.Translate s))

        member __.Append (statement : DeviationStatement) =
            let deviation, body = statement
            indent(); Printf.bprintf sb "deviation %s" (ToYangString deviation.Value)
            block_o body (fun s -> this.Append (DeviationBodyStatement.Translate s))

        member __.Append (statement : EnumStatement) =
            let enum, body = statement
            indent (); Printf.bprintf sb "enum %s" (ToYangString enum)
            block_o body (fun s -> this.Append (EnumBodyStatement.Translate s))

        member __.Append (statement : ExtensionStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "extension %s" id.Value
            block_o body (fun s -> this.Append (ExtensionBodyStatement.Translate s))

        member __.Append (statement : FeatureStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "feature %s" id.Value
            block_o body (fun s -> this.Append (FeatureBodyStatement.Translate s))

        member __.Append (statement : FractionDigitsStatement) =
            let fraction, extra = statement
            indent(); Printf.bprintf sb "fraction-digits %d" fraction
            this.Append extra

        member __.Append (statement : GroupingStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "grouping %s" id.Value
            block_o body (fun s -> this.Append (GroupingBodyStatement.Translate s))

        member __.Append (statement : IdentityStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "identity %s" id.Value
            block_o body (fun s -> this.Append (IdentityBodyStatement.Translate s))

        member __.Append (statement : IfFeatureStatement) =
            let expression, extra = statement
            indent(); Printf.bprintf sb "if-feature "; (Expressions.PrettyPrint (sb, indentation) expression)
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
            block_o body (fun s -> this.Append (IncludeBodyStatement.Translate s))

        member __.Append (body : InputStatement) =
            indent(); Printf.bprintf sb "input"
            block body (fun s -> this.Append (InputBodyStatement.Translate s))

        member __.Append (statement : KeyStatement) =
            let key, extra = statement
            indent(); Printf.bprintf sb "key %s" (ToYangString (Arguments.Key.Value key))
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
            indent(); Printf.bprintf sb "length %s" (ToYangString id.Value)
            block_o body (fun s -> this.Append (LengthBodyStatement.Translate s))

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
            indent(); Printf.bprintf sb "max-elements %s" (ToYangString max.Value)
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
            indent(); Printf.bprintf sb "min-elements %s" (ToYangString min.Value)
            this.Append extra

        member __.Append (statement : ModuleStatement) =
            indent ()
            Printf.bprintf sb "module %s {" statement.Name.Value
            nl()
            indentation <- indentation + 1

            let version, ns, prefix, unknowns = statement.Header
            this.Append version
            this.Append ns
            this.Append prefix
            if unknowns.IsSome then indent(); this.Append unknowns

            let linkage = statement.Linkage
            if linkage.IsEmpty = false then
                // leave an empty line
                nl()
                linkage |> List.iter (this.Append)

            let meta = statement.Meta
            if meta.Length > 0 then
                nl()
                meta |> List.iter (this.Append)

            let revision = statement.Revision
            if revision.IsEmpty = false then
                nl()
                revision |> List.iter (this.Append)

            let body = statement.Body
            if body.IsEmpty = false then
                nl()
                body |> List.iter (this.Append)

            indentation <- indentation - 1
            indent ()
            Printf.bprintf sb "}"
            nl()

        member __.Append (statement : ModifierStatement) =
            let modifier, extra = statement
            indent(); Printf.bprintf sb "modifier %s" (ToYangString modifier.Value)
            this.Append extra

        member __.Append (statement : MustStatement) =
            let condition, body = statement
            indent(); Printf.bprintf sb "must %s" (ToYangString condition)
            block_o body (fun s -> this.Append (MustBodyStatement.Translate s))

        member __.Append (statement : NamespaceStatement) =
            let uri, extra = statement
            indent ()
            Printf.bprintf sb "namespace \"%s\"" (uri.ToString())
            this.Append extra

        member __.Append (statement : NotificationStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "notification %s" id.Value
            block_o body (fun s -> this.Append (NotificationBodyStatement.Translate s))

        member __.Append (statement : OrderedByStatement) =
            let order, extra = statement
            indent(); Printf.bprintf sb "must %s" (ToYangString order.Value)
            this.Append extra

        member __.Append (body : OutputStatement) =
            indent(); Printf.bprintf sb "output"
            block body (fun s -> this.Append (OutputBodyStatement.Translate s))

        member __.Append (statement : PathStatement) =
            let path, extra = statement
            indent ()
            Printf.bprintf sb "path %s" (ToYangString path.Value)
            this.Append extra

        member __.Append (statement : PatternStatement) =
            let pattern, body = statement
            indent(); Printf.bprintf sb "pattern %s" (ToYangStringQuoted pattern)
            block_o body (fun s -> this.Append (PatternBodyStatement.Translate s))

        member __.Append (statement : PrefixStatement) =
            let prefix, extra = statement
            indent ()
            Printf.bprintf sb "prefix %s" (ToYangStringQuoted prefix)
            this.Append extra

        member __.Append (statement : PositionStatement) =
            let position, extra = statement
            indent(); Printf.bprintf sb "position %d" position
            this.Append extra

        member __.Append (statement : RangeStatement) =
            let range, body = statement
            indent(); Printf.bprintf sb "range %s" (ToYangString range.Value)
            block_o body (fun s -> this.Append (RangeBodyStatement.Translate s))

        member __.Append (statement : RefineStatement) =
            let refine, body = statement
            indent(); Printf.bprintf sb "range %s" (ToYangString refine.Value)
            block_o body (fun s -> this.Append (RefineBodyStatement.Translate s))

        member __.Append (statement : RevisionBodyStatement) =
            match statement with
            | RevisionBodyStatement.Description st  -> this.Append st
            | RevisionBodyStatement.Reference   st  -> this.Append st
            | RevisionBodyStatement.Unknown     st  -> this.Append st

        member __.Append (statement : RevisionStatement) =
            let date, body = statement
            indent (); Printf.bprintf sb "revision %s" (ToYangString date.Value)
            block_o body (fun s -> this.Append (RevisionBodyStatement.Translate s))

        member __.Append (statement : RevisionDateStatement) =
            let date, extra = statement;
            indent ()
            Printf.bprintf sb "revision %s" (ToYangString date.Value)
            this.Append extra

        member __.Append (statement : RpcStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "rpc %s" id.Value
            block_o body (fun s -> this.Append (RpcBodyStatement.Translate s))

        member __.Append (statement : StatusStatement) =
            let status, extra = statement
            indent(); Printf.bprintf sb "status %s" (ToYangString status.Value)
            this.Append extra

        member __.Append (statement : SubmoduleStatement) =
            indent ()
            Printf.bprintf sb "submodule %s {" statement.Name.Value
            nl()
            indentation <- indentation + 1

            let version, belongsTo, unknowns = statement.Header
            this.Append version
            this.Append belongsTo
            this.Append unknowns

            let linkage = statement.Linkage
            if linkage.IsEmpty = false then
                // leave an empty line
                nl()
                linkage |> List.iter (this.Append)

            let meta = statement.Meta
            if meta.Length > 0 then
                nl()
                meta |> List.iter (this.Append)

            let revision = statement.Revision
            if revision.IsEmpty = false then
                nl()
                revision |> List.iter (this.Append)

            let body = statement.Body
            if body.IsEmpty = false then
                nl()
                body |> List.iter (this.Append)

            indentation <- indentation - 1
            indent ()
            Printf.bprintf sb "}"
            nl()


        member __.Append (statement : TypeStatement) =
            let id, arg, body = statement
            indent(); Printf.bprintf sb "type %s" id.Value

            if arg.IsNone && body.IsNone then Printf.bprintf sb ";"; nl()
            elif arg.IsNone && body.Value.Length = 0 then Printf.bprintf sb " {}"; nl()
            else
                Printf.bprintf sb "{"
                nl()
                indent ()

                if arg.IsSome then
                    // TODO: Pretty print TypeBodyStatement
                    Printf.bprintf sb " %A" arg.Value
                    nl()
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
            indent(); Printf.bprintf sb "unique %s" (ToYangString unique.Value)
            this.Append extra

        member __.Append (statement : UsesStatement) =
            let id, body = statement
            indent(); Printf.bprintf sb "uses %s" id.Value
            block_o body (fun s -> this.Append (UsesBodyStatement.Translate s))

        member __.Append (statement : UsesAugmentStatement) =
            let augment, body = statement
            indent(); Printf.bprintf sb "augment %s" (ToYangString augment.Value)
            block body (fun s -> this.Append (UsesAugmentBodyStatement.Translate s))

        member __.Append (statement : ValueStatement) =
            let value, extra = statement
            indent(); Printf.bprintf sb "value %d" value
            this.Append extra

        member __.Append (statement : WhenStatement) =
            let condition, body = statement
            indent(); Printf.bprintf sb "when %s" (ToYangString condition)
            block_o body (fun s -> this.Append (WhenBodyStatement.Translate s))

        member __.Append (statement : YangVersionStatement) =
            let version, extra = statement
            indent ()
            Printf.bprintf sb "yang-version %d.%d" version.Major version.Minor
            this.Append extra

        member __.Append (statement : Statement) =
            if mode._IsNextSuppress then
                mode <- Suppressed
            elif compact && (StatementHelper.CountDescendants statement) = 1 then
                mode <- NextSuppress

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

            if mode._IsSuppressed then mode <- NextSuppress
            elif mode._IsNextSuppress then mode <- Normal

        member __.Append (statement : ExtraStatements) =
            match statement with
            | None              -> Printf.bprintf sb ";"; nl()
            | Some statements   ->
                Printf.bprintf sb " {"
                nl()
                indentation <- indentation + 1

                statements |> List.iter (this.Append)

                indentation <- indentation - 1
                indent ()
                Printf.bprintf sb "}"
                nl()

        member __.Append (statement : UnknownStatement) =
            let id, arg, extra = statement
            Printf.bprintf sb "%s" (id.ToString())
            if arg.IsSome then Printf.bprintf sb " %s" (ToYangString (arg.Value))
            this.Append extra

        member __.Append (unknowns : UnknownStatement list option) =
            match unknowns with
            | None          -> Printf.bprintf sb ";"; nl()
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

    let ModuleToStringCompact (statement : ModuleStatement) =
        let printer = YangPrinter (compact=true)
        printer.Append statement
        printer.ToString()

    let StatementToString (statement : Statement) =
        let printer = YangPrinter ()
        printer.Append statement
        printer.ToString ()

    let StatementToStringCompact (statement : Statement) =
        let printer = YangPrinter (compact=true)
        printer.Append statement
        printer.ToString ()

#if INTERACTIVE
    let UsePrettyPrinter ()     = Statements.StatementPrinter.Set StatementToStringCompact
    let ClearPrettyPrinter ()   = Statements.StatementPrinter.Reset()
#endif

    do
        Statements.StatementPrinter.Set StatementToString
