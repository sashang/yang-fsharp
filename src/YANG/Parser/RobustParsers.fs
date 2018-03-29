// RobustParsers.fs
namespace Yang.Parser

[<AutoOpen>]
module RobustParsers =
    open FParsec
    open Yang.Model

    // The following parsers accept values that can appear without quotes, with single quotes,
    // or with double quotes. Moreover, when using single quotes or double quotes, the input
    // string can be the concatenation of multiple strings.
    //  The easiest approach is to first read the string, and then apply the parser on that
    // string. However, this seems to be rather wasteful. Eventually, for some of the parsers
    // below, we may need to combine the parsers to gain some performance.

    // TODO: Evaluate performance gains by combining the parsers below

    let parse_identifier<'a> : Parser<Identifier, 'a> =
        pip Strings.parse_string Identifier.parse_identifier

    let parse_identifier_reference<'a> : Parser<IdentifierReference, 'a> =
        pip Strings.parse_string Identifier.parse_identifier_reference

    let parse_boolean<'a> : Parser<bool, 'a> =
        pip Strings.parse_string Arguments.parse_boolean

    let parse_date<'a> : Parser<Arguments.Date, 'a> =
        pip Strings.parse_string Arguments.parse_date

    let parse_length<'a> : Parser<Arguments.Length.Length, 'a> =
        pip Strings.parse_string Arguments.parse_length

    let parse_max_value<'a> : Parser<Arguments.MaxValue, 'a> =
        pip Strings.parse_string Arguments.parse_max_value

    let parse_modifier<'a> : Parser<Arguments.Modifier, 'a> =
        pip Strings.parse_string Arguments.parse_modifier

    let parse_min_value<'a> : Parser<Arguments.MinValue, 'a> =
        pip Strings.parse_string Arguments.parse_min_value

    let parse_ordered_by<'a> : Parser<Arguments.OrderedBy, 'a> =
        pip Strings.parse_string Arguments.parse_ordered_by

    let parse_path<'a> : Parser<Arguments.Path.Path, 'a> =
        pip Strings.parse_string parse_path

    let parse_range<'a> : Parser<Arguments.Range.Range, 'a> =
        pip Strings.parse_string Arguments.parse_range

    let parse_schema_node_identifier_absolute<'a> : Parser<SchemaNodeIdentifier, 'a> =
        pip Strings.parse_string Identifier.parse_schema_node_identifier_absolute

    let parse_schema_node_identifier_descendant<'a> : Parser<SchemaNodeIdentifier, 'a> =
        pip Strings.parse_string Identifier.parse_schema_node_identifier_descendant

    let parse_status<'a> : Parser<Arguments.Status, 'a> =
        pip Strings.parse_string Arguments.parse_status

    let parse_unique<'a> : Parser<Arguments.Unique, 'a> =
        pip Strings.parse_string Identifier.parse_unique

    let parse_int64<'a> : Parser<int64, 'a> =
        pip Strings.parse_string pint64

    let parse_uint32<'a> : Parser<uint32, 'a> =
        pip Strings.parse_string puint32

    let parse_skipString<'a> (keyword : string) : Parser<unit, 'a> =
        pip Strings.parse_string (skipString keyword)

