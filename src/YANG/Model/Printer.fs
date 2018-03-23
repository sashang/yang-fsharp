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

        // The following definitions need to be defined here, because the compiler
        // cannot find them when they are used before they are defined as members.
        // (Not sure why this is happening though)

        let AppendNamespace (NamespaceStatement (uri, extra)) =
            indent ()
            Printf.bprintf sb "namespace \"%s\"" (uri.ToString())
            this.Append extra

        let AppendOrganization (OrganizationStatement (organization, extra)) =
            indent(); Printf.bprintf sb "organization %s" (ToYangString organization)
            this.Append extra

        let AppendPrefix (PrefixStatement (prefix, extra)) =
            indent (); Printf.bprintf sb "prefix %s" (ToYangStringQuoted prefix)
            this.Append extra

        let AppendReference (ReferenceStatement (reference, extra)) =
            indent(); Printf.bprintf sb "reference %s" (ToYangString reference)
            this.Append extra

        let AppendRevision (RevisionStatement (date, body)) =
            indent (); Printf.bprintf sb "revision %s" (ToYangString date.Value)
            block_o body (fun s -> this.Append (RevisionBodyStatement.Translate s))

        let AppendRevisionDate (RevisionDateStatement (date, extra)) =
            indent ()
            Printf.bprintf sb "revision %s" (ToYangString date.Value)
            this.Append extra

        let AppendType (TypeStatement (id, arg)) =
            indent(); Printf.bprintf sb "type %s" id.Value

            if arg.IsNone then Printf.bprintf sb ";"; nl()
            elif arg.IsNone && (TypeBodyStatement.Length arg.Value) = 0 then Printf.bprintf sb " {}"; nl()
            else
                Printf.bprintf sb "{"
                nl()
                indent ()

                if arg.IsSome then this.Append(arg.Value)

        let AppendUnknown (UnknownStatement (id, arg, extra)) =
            Printf.bprintf sb "%s" (id.ToString())
            if arg.IsSome then Printf.bprintf sb " %s" (ToYangString (arg.Value))
            this.Append extra

        let AppendYangVersion (YangVersionStatement (version, extra)) =
            indent (); Printf.bprintf sb "yang-version %d.%d" version.Major version.Minor
            this.Append extra

        member __.Append (ActionStatement (id, body)) =
            indent (); Printf.bprintf sb "action %s" id.Value
            block_o body (fun s -> this.Append (ActionBodyStatement.Translate s))

        member __.Append (AnyDataStatement (id, body)) =
            indent (); Printf.bprintf sb "anydata %s" id.Value
            block_o body (fun s -> this.Append (AnyDataBodyStatement.Translate s))

        member __.Append (AnyXmlStatement (id, body)) =
            indent (); Printf.bprintf sb "anyxml %s" id.Value
            block_o body (fun s -> this.Append (AnyXmlBodyStatement.Translate s))

        member __.Append (ArgumentStatement (id, body)) =
            indent (); Printf.bprintf sb "argument %s" id.Value
            block_o body (fun s -> this.Append (ArgumentBodyStatement.Translate s))

        member __.Append (AugmentStatement (augment, body)) =
            indent (); Printf.bprintf sb "augment %s" (ToYangStringQuoted augment.Value)
            block body (fun s -> this.Append (AugmentBodyStatement.Translate s))

        member __.Append (BelongsToStatement (id, body)) =
            indent (); Printf.bprintf sb "belongs-to %s" id.Value
            block body (fun s -> this.Append (BelongsToBodyStatement.Translate s))

        member __.Append (BaseStatement (id, extra)) =
            indent (); Printf.bprintf sb "base %s" id.Value
            this.Append extra

        member __.Append (BitStatement (id, body)) =
            indent (); Printf.bprintf sb "bit %s" id.Value
            block_o body (fun s -> this.Append (BitBodyStatement.Translate s))

        member __.Append (statement : BodyStatement) =
            this.Append (BodyStatement.Translate statement)

        member __.Append (ContactStatement (contact, extra)) =
            indent (); Printf.bprintf sb "contact %s" (ToYangString contact)
            this.Append extra

        member __.Append (CaseStatement (id, body)) =
            indent (); Printf.bprintf sb "case %s" id.Value
            block_o body (fun s -> this.Append (CaseBodyStatement.Translate s))

        member __.Append (ChoiceStatement (id, body)) =
            indent(); Printf.bprintf sb "choice %s" id.Value
            block_o body (fun s -> this.Append (ChoiceBodyStatement.Translate s))

        member __.Append (ConfigStatement (b, extra)) =
            indent (); Printf.bprintf sb "config %s" (if b then "true" else "false")
            this.Append extra

        member __.Append (ContainerStatement (id, body)) =
            indent(); Printf.bprintf sb "container %s" id.Value
            block_o body (fun s -> this.Append (ContainerBodyStatement.Translate s))

        member __.Append (DefaultStatement (value, extra)) =
            indent(); Printf.bprintf sb "default %s" (ToYangString value)
            this.Append extra

        member __.Append (DescriptionStatement (description, extra)) =
            indent(); Printf.bprintf sb "description %s" (ToYangString description)
            this.Append extra

        member __.Append (DeviateAddStatement body) =
            indent(); Printf.bprintf sb "deviate add"
            block_o body (fun s -> this.Append (DeviateAddBodyStatement.Translate s))

        member __.Append (DeviateDeleteStatement body) =
            indent(); Printf.bprintf sb "deviate delete"
            block_o body (fun s -> this.Append (DeviateDeleteBodyStatement.Translate s))

        member __.Append (DeviateNotSupportedStatement extra) =
            indent(); Printf.bprintf sb "deviate not-supported"
            this.Append extra

        member __.Append (DeviateReplaceStatement body) =
            indent(); Printf.bprintf sb "deviate replace"
            block_o body (fun s -> this.Append (DeviateReplaceBodyStatement.Translate s))

        member __.Append (DeviationStatement (deviation, body)) =
            indent(); Printf.bprintf sb "deviation %s" (ToYangString deviation.Value)
            block body (fun s -> this.Append (DeviationBodyStatement.Translate s))

        member __.Append (EnumStatement (enum, body)) =
            indent (); Printf.bprintf sb "enum %s" (ToYangString enum)
            block_o body (fun s -> this.Append (EnumBodyStatement.Translate s))

        member __.Append (ErrorAppTagStatement (error, extra)) =
            indent (); Printf.bprintf sb "error-app-tag %s" (ToYangString error)
            this.Append extra

        member __.Append (ErrorMessageStatement (error, extra)) =
            indent (); Printf.bprintf sb "error-message %s" (ToYangString error)
            this.Append extra

        member __.Append (ExtensionStatement (id, body)) =
            indent(); Printf.bprintf sb "extension %s" id.Value
            block_o body (fun s -> this.Append (ExtensionBodyStatement.Translate s))

        member __.Append (FeatureStatement (id, body)) =
            indent(); Printf.bprintf sb "feature %s" id.Value
            block_o body (fun s -> this.Append (FeatureBodyStatement.Translate s))

        member __.Append (FractionDigitsStatement (fraction, extra)) =
            indent(); Printf.bprintf sb "fraction-digits %d" fraction
            this.Append extra

        member __.Append (GroupingStatement (id, body)) =
            indent(); Printf.bprintf sb "grouping %s" id.Value
            block_o body (fun s -> this.Append (GroupingBodyStatement.Translate s))

        member __.Append (IdentityStatement (id, body)) =
            indent(); Printf.bprintf sb "identity %s" id.Value
            block_o body (fun s -> this.Append (IdentityBodyStatement.Translate s))

        member __.Append (IfFeatureStatement (expression, extra)) =
            indent(); Printf.bprintf sb "if-feature "; (Expressions.PrettyPrint (sb, indentation) expression)
            this.Append extra

        member __.Append (statement : ImportBodyStatement) =
             match statement with
             | ImportBodyStatement.Description st   -> this.Append st
             | ImportBodyStatement.Prefix st        -> AppendPrefix st
             | ImportBodyStatement.Reference st     -> AppendReference st
             | ImportBodyStatement.RevisionDate st  -> AppendRevisionDate st
             | ImportBodyStatement.Unknown st       -> AppendUnknown st

        member __.Append (ImportStatement (id, body)) =
            indent (); Printf.bprintf sb "import %s" id.Value
            block body (fun s -> this.Append (ImportBodyStatement.Translate s))

        member __.Append (statement : IncludeBodyStatement) =
            match statement with
            | IncludeBodyStatement.Description  st  -> this.Append st
            | IncludeBodyStatement.Reference    st  -> AppendReference st
            | IncludeBodyStatement.RevisionDate st  -> AppendRevisionDate st
            | IncludeBodyStatement.Unknown      st  -> AppendUnknown st

        member __.Append (IncludeStatement (id, body)) =
            indent (); Printf.bprintf sb "include %s" id.Value
            block_o body (fun s -> this.Append (IncludeBodyStatement.Translate s))

        member __.Append (InputStatement body) =
            indent(); Printf.bprintf sb "input"
            block body (fun s -> this.Append (InputBodyStatement.Translate s))

        member __.Append (KeyStatement (key, extra)) =
            indent(); Printf.bprintf sb "key %s" (ToYangString key.Value)
            this.Append extra

        member __.Append (LeafStatement (id, body)) =
            indent(); Printf.bprintf sb "leaf %s" id.Value
            block body (fun s -> this.Append (LeafBodyStatement.Translate s))

        member __.Append (LeafListStatement (id, body)) =
            indent(); Printf.bprintf sb "leaf-list %s" id.Value
            block body (fun s -> this.Append (LeafListBodyStatement.Translate s))

        member __.Append (LengthStatement (id, body)) =
            indent(); Printf.bprintf sb "length %s" (ToYangString id.Value)
            block_o body (fun s -> this.Append (LengthBodyStatement.Translate s))

        member __.Append (statement : LinkageBodyStatement) =
            match statement with
            | Import statement  -> this.Append statement
            | Include statement -> this.Append statement

        member __.Append (ListStatement (id, body)) =
            indent(); Printf.bprintf sb "list %s" id.Value
            block body (fun s -> this.Append (ListBodyStatement.Translate s))

        member __.Append (MandatoryStatement (mandatory, extra)) =
            indent(); Printf.bprintf sb "mandatory %s" (if mandatory then "true" else "false")
            this.Append extra

        member __.Append (MaxElementsStatement (max, extra)) =
            indent(); Printf.bprintf sb "max-elements %s" (ToYangString max.Value)
            this.Append extra

        member __.Append (statement : MetaBodyStatement) =
            match statement with
            | MetaBodyStatement.Contact st      -> this.Append st
            | MetaBodyStatement.Description st  -> this.Append st
            | MetaBodyStatement.Organization st -> AppendOrganization st
            | MetaBodyStatement.Reference   st  -> AppendReference st
            | MetaBodyStatement.Unknown     st  -> AppendUnknown st

        member __.Append (MinElementsStatement (min, extra)) =
            indent(); Printf.bprintf sb "min-elements %s" (ToYangString min.Value)
            this.Append extra

        member __.Append (statement : ModuleStatement) =
            indent ()
            Printf.bprintf sb "module %s {" statement.Name.Value
            nl()
            indentation <- indentation + 1

            let version, ns, prefix, unknowns = statement.Header
            AppendYangVersion version
            AppendNamespace ns
            AppendPrefix prefix
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
                revision |> List.iter AppendRevision

            let body = statement.Body
            if body.IsEmpty = false then
                nl()
                body |> List.iter (this.Append)

            indentation <- indentation - 1
            indent ()
            Printf.bprintf sb "}"
            nl()

        member __.Append (ModifierStatement (modifier, extra)) =
            indent(); Printf.bprintf sb "modifier %s" (ToYangString modifier.Value)
            this.Append extra

        member __.Append (MustStatement (condition, body)) =
            indent(); Printf.bprintf sb "must %s" (ToYangString condition)
            block_o body (fun s -> this.Append (MustBodyStatement.Translate s))

        member __.Append (statement : NamespaceStatement) = AppendNamespace statement

        member __.Append (NotificationStatement (id, body)) =
            indent(); Printf.bprintf sb "notification %s" id.Value
            block_o body (fun s -> this.Append (NotificationBodyStatement.Translate s))

        member __.Append (OrderedByStatement (order, extra)) =
            indent(); Printf.bprintf sb "must %s" (ToYangString order.Value)
            this.Append extra

        member __.append (statement : OrganizationStatement) = AppendOrganization statement

        member __.Append (OutputStatement body) =
            indent(); Printf.bprintf sb "output"
            block body (fun s -> this.Append (OutputBodyStatement.Translate s))

        member __.Append (PathStatement (path, extra)) =
            indent (); Printf.bprintf sb "path %s" (ToYangString path.Value)
            this.Append extra

        member __.Append (PatternStatement (pattern, body)) =
            indent(); Printf.bprintf sb "pattern %s" (ToYangStringQuoted pattern)
            block_o body (fun s -> this.Append (PatternBodyStatement.Translate s))

        member __.Append (statement : PrefixStatement) = AppendPrefix statement

        member __.Append (PresenceStatement (presence, extra)) =
            indent(); Printf.bprintf sb "prefix %s" (ToYangStringQuoted presence)
            this.Append extra

        member __.Append (PositionStatement (position, extra)) =
            indent(); Printf.bprintf sb "position %d" position
            this.Append extra

        member __.Append (RangeStatement (range, body)) =
            indent(); Printf.bprintf sb "range %s" (ToYangString range.Value)
            block_o body (fun s -> this.Append (RangeBodyStatement.Translate s))

        member __.Append (statement : ReferenceStatement) = AppendReference statement

        member __.Append (RefineStatement (refine, body)) =
            indent(); Printf.bprintf sb "refine %s" (ToYangString refine.Value)
            block_o body (fun s -> this.Append (RefineBodyStatement.Translate s))

        member __.Append (RequireInstanceStatement (require, extra)) =
            indent(); Printf.bprintf sb "require %s" (if require then "true" else "false")
            this.Append extra

        member __.Append (statement : RevisionBodyStatement) =
            match statement with
            | RevisionBodyStatement.Description st  -> this.Append st
            | RevisionBodyStatement.Reference   st  -> this.Append st
            | RevisionBodyStatement.Unknown     st  -> AppendUnknown st

        member __.Append (statement : RevisionStatement) = AppendRevision statement

        member __.Append (statement : RevisionDateStatement) = AppendRevisionDate statement

        member __.Append (RpcStatement (id, body)) =
            indent(); Printf.bprintf sb "rpc %s" id.Value
            block_o body (fun s -> this.Append (RpcBodyStatement.Translate s))

        member __.Append (StatusStatement (status, extra)) =
            indent(); Printf.bprintf sb "status %s" (ToYangString status.Value)
            this.Append extra

        member __.Append (statement : SubmoduleStatement) =
            indent ()
            Printf.bprintf sb "submodule %s {" statement.Name.Value
            nl()
            indentation <- indentation + 1

            let version, belongsTo, unknowns = statement.Header
            AppendYangVersion version
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

        member __.Append (specification : BinarySpecification) =
            specification
            |> List.iter (
                fun spec ->
                    match spec with
                    | BinaryBodySpecification.Length    length  -> this.Append length
                    | BinaryBodySpecification.Unknown   unknown -> AppendUnknown unknown

                    nl ()
            )

        member __.Append (specification : BitsSpecification) =
            specification
            |> List.iter (
                fun spec ->
                    match spec with
                    | BitsBodySpecification.Bit     bit         -> this.Append bit
                    | BitsBodySpecification.Unknown unknown     -> AppendUnknown unknown

                    nl ()
            )

        member __.Append (specification : Decimal64Specification) =
            specification
            |> List.iter (
                fun spec ->
                    match spec with
                    | Decimal64BodySpecification.FractionDigits fraction    -> this.Append fraction
                    | Decimal64BodySpecification.Range          range       -> this.Append range
                    | Decimal64BodySpecification.Unknown        unknown     -> AppendUnknown unknown

                    nl ()
            )

        member __.Append (specification : EnumSpecification) =
            specification
            |> List.iter (
                fun spec ->
                    match spec with
                    | EnumBodySpecification.Enum    enum                    -> this.Append enum
                    | EnumBodySpecification.Unknown unknown                 -> AppendUnknown unknown

                    nl()
            )

        member __.Append (specification : IdentityRefSpecification) =
            specification
            |> List.iter (
                fun spec ->
                    match spec with
                    | IdentityRefBodySpecification.Base     ``base``        -> this.Append ``base``
                    | IdentityRefBodySpecification.Unknown  unknown         -> AppendUnknown unknown

                    nl()
            )

        member __.Append (specification : InstanceIdentifierSpecification) =
            specification
            |> List.iter (
                fun spec ->
                    match spec with
                    | InstanceIdentifierBodySpecification.RequireInstance   require -> this.Append require
                    | InstanceIdentifierBodySpecification.Unknown           unknown -> AppendUnknown unknown

                    nl()
            )

        member __.Append (specification : LeafRefSpecification) =
            specification
            |> List.iter (
                fun spec ->
                    match spec with
                    | LeafRefBodySpecification.Path     path                -> this.Append path
                    | LeafRefBodySpecification.Require  require             -> this.Append require
                    | LeafRefBodySpecification.Unknown  unknown             -> AppendUnknown unknown

                    nl()
            )

        member __.Append (specification : NumericalRestrictions) =
            specification
            |> List.iter (
                fun spec ->
                    match spec with
                    | NumericalBodyRestrictions.Range   range               -> this.Append range
                    | NumericalBodyRestrictions.Unknown unknown             -> AppendUnknown unknown

                    nl()
            )

        member __.Append (specification : StringRestrictions) =
            specification
            |> List.iter (
                fun spec ->
                    match spec with
                    | StringBodyRestrictions.Length     length              -> this.Append length
                    | StringBodyRestrictions.Pattern    pattern             -> this.Append pattern
                    | StringBodyRestrictions.Unknown    unknown             -> AppendUnknown unknown

                    nl()
            )

        member __.Append (specification : UnionSpecification) =
            specification
            |> List.iter (
                fun spec ->
                    match spec with
                    | UnionBodySpecification.Type       ``type``            -> AppendType ``type``
                    | UnionBodySpecification.Unknown    unknown             -> AppendUnknown unknown

                    nl()
            )

        member __.Append (statement : TypeBodyStatement) =
            match statement with
            | TypeBodyStatement.BinarySpecification             arg -> this.Append arg
            | TypeBodyStatement.BitsSpecification               arg -> this.Append arg
            | TypeBodyStatement.Decimal64Specification          arg -> this.Append arg
            | TypeBodyStatement.EnumSpecification               arg -> this.Append arg
            | TypeBodyStatement.IdentityRefSpecification        arg -> this.Append arg
            | TypeBodyStatement.InstanceIdentifierSpecification arg -> this.Append arg
            | TypeBodyStatement.LeafRefSpecification            arg -> this.Append arg
            | TypeBodyStatement.NumericalRestrictions           arg -> this.Append arg
            | TypeBodyStatement.StringRestrictions              arg -> this.Append arg
            | TypeBodyStatement.UnionSpecification              arg -> this.Append arg
            | TypeBodyStatement.UnknownTypeSpecification        arg ->
                arg |> List.iter (fun st -> this.Append st; nl())

        member __.Append (statement : TypeStatement) =  AppendType statement

        member __.Append (TypeDefStatement (id, body)) =
            indent(); Printf.bprintf sb "typedef %s" id.Value
            block body (fun s -> this.Append (TypeDefBodyStatement.Translate s))

        member __.Append (UnitsStatement (units, extra)) =
            indent(); Printf.bprintf sb "units %s" (ToYangString units)
            this.Append extra

        member __.Append (UniqueStatement (unique, extra)) =
            indent(); Printf.bprintf sb "unique %s" (ToYangString unique.Value)
            this.Append extra

        member __.Append (UsesStatement (id, body)) =
            indent(); Printf.bprintf sb "uses %s" id.Value
            block_o body (fun s -> this.Append (UsesBodyStatement.Translate s))

        member __.Append (UsesAugmentStatement (augment, body)) =
            indent(); Printf.bprintf sb "augment %s" (ToYangString augment.Value)
            block body (fun s -> this.Append (UsesAugmentBodyStatement.Translate s))

        member __.Append (ValueStatement (value, extra)) =
            indent(); Printf.bprintf sb "value %d" value
            this.Append extra

        member __.Append (WhenStatement (condition, body)) =
            indent(); Printf.bprintf sb "when %s" (ToYangString condition)
            block_o body (fun s -> this.Append (WhenBodyStatement.Translate s))

        member __.Append (statement : YangVersionStatement) = AppendYangVersion statement

        member __.Append (YinElementStatement (yin, extra)) =
            indent (); Printf.bprintf sb "yin-element %s" (if yin then "true" else "false")
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
            | Statement.Namespace st            -> AppendNamespace st
            | Statement.Notification st         -> this.Append st
            | Statement.OrderedBy st            -> this.Append st
            | Statement.Organization st         -> AppendOrganization st
            | Statement.Output st               -> this.Append st
            | Statement.Path st                 -> this.Append st
            | Statement.Pattern st              -> this.Append st
            | Statement.Position st             -> this.Append st
            | Statement.Prefix st               -> AppendPrefix st
            | Statement.Presence st             -> this.Append st
            | Statement.Range st                -> this.Append st
            | Statement.Reference st            -> AppendReference st
            | Statement.Refine st               -> this.Append st
            | Statement.Revision st             -> AppendRevision st
            | Statement.RevisionDate st         -> AppendRevisionDate st
            | Statement.RequireInstance st      -> this.Append st
            | Statement.Rpc st                  -> this.Append st
            | Statement.Status st               -> this.Append st
            | Statement.Submodule st            -> this.Append st
            | Statement.Type st                 -> AppendType st
            | Statement.TypeDef st              -> this.Append st
            | Statement.Unique st               -> this.Append st
            | Statement.Units st                -> this.Append st
            | Statement.Unknown st              -> AppendUnknown st
            | Statement.Uses st                 -> this.Append st
            | Statement.UsesAugment st          -> this.Append st
            | Statement.Value st                -> this.Append st
            | Statement.When st                 -> this.Append st
            | Statement.YangVersion st          -> AppendYangVersion st
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

        member __.Append (statement : UnknownStatement) = AppendUnknown statement

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

