// GenericParser.fs
// Parser for generic blocks, ie. blocks that can contain any statement

namespace Yang.Parser

[<AutoOpen>]
module GenericParser =
    open FParsec
    open NLog
    open Yang.Model
    open Yang.Parser.BodyStatements
    open Yang.Parser.Expressions
    open Yang.Parser.Leaf
    open Yang.Parser.LeafList
    open Yang.Parser.Linkage
    open Yang.Parser.Module
    open Yang.Parser.Types

    /// Logger for this module
    let private _logger = LogManager.GetCurrentClassLogger()

    let private throw fmt =
        let do_throw (message : string) =
            _logger.Error message
            raise (YangParserException message)
        Printf.ksprintf do_throw fmt

    let private warn fmt = Printf.ksprintf _logger.Warn fmt
    let private debug fmt = Printf.ksprintf _logger.Debug fmt
    let private trace fmt = Printf.ksprintf _logger.Trace fmt
    let private error fmt = Printf.ksprintf _logger.Error fmt

#if INTERACTIVE
    // The following are used only in interactive (fsi) to help with enabling disabling
    // logging for particular modules.

    type internal Marker = interface end
    let _full_name = typeof<Marker>.DeclaringType.FullName
    let _name = typeof<Marker>.DeclaringType.Name
#endif

    let generic_yang_statement_implementation<'a> : Parser<Statement, 'a> =
            (parse_action_statement             |>> Statement.Action)
        <|> (parse_any_data_statement           |>> Statement.AnyData)
        <|> (parse_any_xml_statement            |>> Statement.AnyXml)
        <|> (parse_argument_statement           |>> Statement.Argument)
        <|> (parse_augment_statement            |>> Statement.Augment)
        <|> (parse_base_statement               |>> Statement.Base)
        <|> (parse_belongs_to_statement         |>> Statement.BelongsTo)
        <|> (parse_bit_statement                |>> Statement.Bit)
        <|> (parse_case_statement               |>> Statement.Case)
        <|> (parse_choice_statement             |>> Statement.Choice)
        <|> (parse_config_statement             |>> Statement.Config)
        <|> (parse_contact_statement            |>> Statement.Contact)
        <|> (parse_container_statement          |>> Statement.Container)
        <|> (parse_default_statement            |>> Statement.Default)
        <|> (parse_description_statement        |>> Statement.Description)

        <|> (parse_enum_statement               |>> Statement.Enum)
        <|> (parse_error_app_tag_statement      |>> Statement.ErrorAppTag)
        <|> (parse_error_message_statement      |>> Statement.ErrorMessage)
        <|> (parse_extension_statement          |>> Statement.Extension)
        <|> (parse_feature_statement            |>> Statement.Feature)
        <|> (parse_fraction_digits_statement    |>> Statement.FractionDigits)
        <|> (parse_grouping_statement           |>> Statement.Grouping)
        <|> (parse_identity_statement           |>> Statement.Identity)
        <|> (parse_if_feature_statement         |>> Statement.IfFeature)
        <|> (parse_import_statement             |>> Statement.Import)
        <|> (parse_include_statement            |>> Statement.Include)
        <|> (parse_input_statement              |>> Statement.Input)
        <|> (parse_key_statement                |>> Statement.Key)
        <|> (parse_leaf_list_statement          |>> Statement.LeafList)
        <|> (parse_leaf_statement               |>> Statement.Leaf)
        <|> (parse_length_statement             |>> Statement.Length)
        <|> (parse_list_statement               |>> Statement.List)
        <|> (parse_mandatory_statement          |>> Statement.Mandatory)
        <|> (parse_max_elements_statement       |>> Statement.MaxElements)
        <|> (parse_min_elements_statement       |>> Statement.MinElements)
        <|> (parse_modifier_statement           |>> Statement.Modifier)
        <|> (parse_module                       |>> Statement.Module)
        <|> (parse_must_statement               |>> Statement.Must)
        <|> (parse_namespace_statement          |>> Statement.Namespace)
        <|> (parse_notification_statement       |>> Statement.Notification)
        <|> (parse_ordered_by_statement         |>> Statement.OrderedBy)
        <|> (parse_organization_statement       |>> Statement.Organization)

        <|> (parse_path_statement               |>> Statement.Path)
        <|> (parse_pattern_statement            |>> Statement.Pattern)
        <|> (parse_position_statement           |>> Statement.Position)
        <|> (parse_prefix_statement             |>> Statement.Prefix)
        <|> (parse_presence_statement           |>> Statement.Presence)
        <|> (parse_range_statement              |>> Statement.Range)
        <|> (parse_reference_statement          |>> Statement.Reference)
        <|> (parse_refine_statement             |>> Statement.Refine)
        <|> (parse_require_instance_statement   |>> Statement.RequireInstance)
        <|> (parse_revision_date_statement      |>> Statement.RevisionDate)
        <|> (parse_revision_statement           |>> Statement.Revision)

        <|> (parse_status_statement             |>> Statement.Status)
        <|> (parse_submodule                    |>> Statement.Submodule)
        <|> (parse_typedef_statement            |>> Statement.TypeDef)
        <|> (parse_type_statement               |>> Statement.Type)
        <|> (parse_unique_statement             |>> Statement.Unique)
        <|> (parse_units_statement              |>> Statement.Units)

        <|> (parse_value_statement              |>> Statement.Value)
        <|> (parse_when_statement               |>> Statement.When)
        <|> (parse_yang_version_statement       |>> Statement.YangVersion)
        <|> (parse_yin_element_statement        |>> Statement.YinElement)
        <|> (parse_unknown_statement            |>> Statement.Unknown)

    type GenericYangStatementParserGenerator<'a> =
        static member Parser() : Parser<Statement, 'a> = generic_yang_statement_implementation

    let private monitor = new System.Object()

    let initialize() =
        if Statements.generic_parser_implementations.ContainsKey(typeof<Microsoft.FSharp.Core.Unit>) = false then
            lock monitor (
                fun () ->
                    if Statements.generic_parser_implementations.ContainsKey(typeof<Microsoft.FSharp.Core.Unit>) = false then
                        trace "Initializing generator for unit"
                        let unit_generic_parser = GenericYangStatementParserGenerator<Microsoft.FSharp.Core.Unit>.Parser()
                        Statements.generic_parser_implementations.Add(typeof<Microsoft.FSharp.Core.Unit>, unit_generic_parser)
                )

        if Statements.generic_parser_generator.IsNone then
            lock monitor (
                fun () ->
                    if Statements.generic_parser_generator.IsNone then
                        trace "Initializing generic generator"
                        Statements.generic_parser_generator <- Some (typeof<GenericYangStatementParserGenerator<unit>>)
            )

