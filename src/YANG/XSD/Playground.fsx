#r "System.Xml.dll"
#r "System.Xml.Linq.dll"

#r @"..\Model\bin\Debug\Yang.Model.dll"

open System
open System.IO
open System.Text
open FSharp.Collections
open System.Xml
open System.Xml.Schema

open Yang.Model
open System.Web.UI.WebControls
open Yang.Model.Arguments

type Namespace = {
    Prefix      : string
    Uri         : Uri
    LocalFile   : string option
}
with
    static member Make(prefix, uri, filename) = {
        Prefix      = prefix
        Uri         = Uri(uri)
        LocalFile   = Some filename
    }

let junos = Namespace.Make ("junos", "http://xml.juniper.net/junos/15.1X24/junos", "junos.xsd")
let junos_xsd = let text = """
<?xml version="1.0" encoding="us-ascii"?>
<xsd:schema elementFormDefault="qualified" \
        attributeFormDefault="unqualified" \
        xmlns:xsd="http://www.w3.org/2001/XMLSchema" \
        targetNamespace="http://xml.juniper.net/junos/Junos-version/junos">
    <xsd:element name="comment" type="xsd:string"/>
</xsd:schema>""" in text.Trim()

if File.Exists("junos.xsd") = false then File.WriteAllText("junos.xsd", junos_xsd)

let error_handler (_ : obj) (args : ValidationEventArgs) =
    printfn "Error (%A) in line (%d,%d) : %s" args.Severity args.Exception.LineNumber args.Exception.LinePosition args.Message



type XmlSchema =
    static member Parse(schema : string, prefix : string, extra : Namespace list) =
        let sb = StringBuilder ()

        Printf.bprintf sb "<%s:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" elementFormDefault=\"qualified\"" prefix
        extra |> List.iter (
            fun ns ->
                Printf.bprintf sb " xmlns:%s=\"%s\"" ns.Prefix (ns.Uri.ToString())
        )
        Printf.bprintf sb ">\n"

        extra |> List.iter (
            fun ns ->
                match ns.LocalFile with
                | None          -> ()
                | Some filename ->
                    Printf.bprintf sb "  <%s:import schemaLocation=\"%s\" namespace=\"%s\"/>\n" prefix (ns.Uri.ToString()) filename
        )

        Printf.bprintf sb "%s\n" (schema.Trim())
        Printf.bprintf sb "</%s:schema>" prefix

        use reader = new StringReader(sb.ToString())
        XmlSchema.Read(reader, error_handler)

    static member Parse(schema : string) = XmlSchema.Parse(schema, "xsd", [])
    static member Parse(schema : string, extra : Namespace list) = XmlSchema.Parse(schema, "xsd", extra)
    static member Parse(schema : string, prefix : string) = XmlSchema.Parse(schema, prefix, [])

module SchemaUtils =
    let head schema =
        let parsed = XmlSchema.Parse schema
        parsed.Items.Item 0

module Patterns =
    let AsComplexType : XmlSchemaObject -> XmlSchemaComplexType option = function
    | :? XmlSchemaComplexType as t -> Some t
    | _                            -> None

    let (|AnyAttribute|Attribute|AttributeGroup|AttributeGroupRef|Object|) (input : XmlSchemaObject) =
        match input with
        | :? XmlSchemaAnyAttribute      as o    -> AnyAttribute         o
        | :? XmlSchemaAttribute         as o    -> Attribute            o
        | :? XmlSchemaAttributeGroup    as o    -> AttributeGroup       o
        | :? XmlSchemaAttributeGroupRef as o    -> AttributeGroupRef    o
        | _                                     -> Object               input

    let (|Complex|Simple|Type|Object|) (input : XmlSchemaObject) =
        match input with
        | :? XmlSchemaComplexType       as o    -> Complex              o
        | :? XmlSchemaSimpleType        as o    -> Simple               o
        | :? XmlSchemaType              as o    -> Type                 o
        | _                                     -> Object               input


let (|StringEqual|) label (s : string) = String.Equals(label, s, StringComparison.InvariantCultureIgnoreCase)


let parse_simple_content_extension (definition : XmlSchemaSimpleContentExtension) =
    let id =
        let type_name = definition.BaseTypeName.Name
        match type_name with
        | StringEqual "string"          true    -> IdentifierReference.Make "string"
        | StringEqual "long"            true    -> IdentifierReference.Make "int64"
        | StringEqual "unsignedLong"    true    -> IdentifierReference.Make "uint64"
        | _ -> failwithf "Unknown type %s" type_name

    let extensions =
        if definition.Attributes = null then None
        else
            definition.Attributes
            |> Seq.cast
            |> Seq.map (
                fun (v : XmlSchemaAttribute) ->
                    printfn "Not handling XmlSchemaAttribute"
                    v
            )
            |> Seq.toList
            |> Some

    id, extensions

let parse_simple_content (definition : XmlSchemaSimpleContent) =
    match definition.Content with
    | :? XmlSchemaSimpleContentExtension as extension ->
        let ty, attributes = parse_simple_content_extension extension
        Statements.TypeStatement (ty, None)
    | _ -> failwithf "Unknown content: %A" definition.Content

let make_flag_statement (flag : string) =
    let prefix = IdentifierWithPrefix.Make "support:flag"
    UnknownStatement (prefix, Some flag, None)

let parse_app_info (definition : XmlSchemaAppInfo) =
    if definition.Markup = null then []
    else
        definition.Markup
        |> Array.toList
        |> List.map (
            fun def ->
                match def.Name with
                | StringEqual "flag" true   -> make_flag_statement def.InnerText
                | _ -> failwithf "Do not know how to parse AppInfo: %A" def
        )

let parse_description (documentation : XmlSchemaDocumentation) =
    let message =
        documentation.Markup
        |> Array.map (fun (entry : XmlNode) -> entry.Value)
        |> String.concat "\n"
    Statements.DescriptionStatement (message, None)

let parse_annotation (definition : XmlSchemaAnnotation) =
    if definition = null then []
    else
        definition.Items
        |> Seq.cast
        |> Seq.collect (
            fun (annotation : XmlSchemaObject) ->
                match annotation with
                | :? XmlSchemaDocumentation as documentation ->
                    [ Statement.Description (parse_description documentation) ]

                | :? XmlSchemaAppInfo as appInfo -> parse_app_info appInfo |> List.map Statement.Unknown

                | _ ->
                    failwithf "Do not know how to parse in annotation (%d, %d): %A"
                        annotation.LineNumber annotation.LinePosition
                        annotation
        )
        |> Seq.toList

let add_no_options<'T> (value : 'T) : 'T * ExtraStatements = value, None

let parse_complex_type (definition : XmlSchemaComplexType) =
    // TODO: When should we define as grouping and when as typedef?

    let name = definition.Name
    assert(String.IsNullOrWhiteSpace(name) = false)
    let identifier = Identifier.Make name

    let documentation =
        parse_annotation definition.Annotation
        |> List.map (TypeDefBodyStatement.FromStatement)

    if definition.ContentModel <> null && definition.Particle = null then
        match definition.ContentModel with
        | :? XmlSchemaSimpleContent as model ->
            let content = parse_simple_content model
            let statements = (TypeDefBodyStatement.Type content) :: documentation
            Statements.TypeDefStatement (identifier, statements)

        | _ -> failwithf "Unknown content model (%d, %d): %A" definition.LineNumber definition.LinePosition (definition.GetType())

    elif definition.ContentModel = null && definition.Particle <> null then
        match definition.Particle with
        | :? XmlSchemaSequence as sequence ->
            let minimumValue : Arguments.MinValue option =
                let get_min_value : decimal -> MinValue = MinValue.Make
                if sequence.MinOccursString = null then None
                else Some (get_min_value sequence.MinOccurs)

            let maximumValue : Arguments.MaxValue option =
                let get_max_value : decimal -> MaxValue = MaxValue.Make
                if sequence.MaxOccursString = null then None
                else Some (get_max_value sequence.MaxOccurs)

            let list_statements =
                [
                   minimumValue |> Option.map (add_no_options >> MinElementsStatement >> ListBodyStatement.MinElements)
                   maximumValue |> Option.map (add_no_options >> MaxElementsStatement >> ListBodyStatement.MaxElements)
                ] |> List.choose id

            // TODO: finish the statement.

            Statements.TypeDefStatement (identifier, [])

        | _ -> failwith "Unknown particlde model (%d, %d): %A" definition.LineNumber definition.LinePosition (definition.GetType())

    else
        failwithf "Do not know how to parse %A" definition

let parse_schema_element (definition : XmlSchemaElement) =
    printfn "Ignoring schema element: %A" definition
    UnknownStatement (IdentifierWithPrefix.Make "xx:xxx", None, None)

let parse (definition : XmlSchemaObject) =
    match definition with
    | :? XmlSchemaElement       as o    -> Statement.Unknown (parse_schema_element o)
    | :? XmlSchemaComplexType   as o    -> Statement.TypeDef (parse_complex_type o)
    | _ -> failwithf "Do not know how to parse: %A" definition

let everything =
    schema.Items
    |> Seq.cast
    |> Seq.map (
        fun item -> parse item
    )
    |> Seq.toList

let example1 = """
  <xsd:complexType name="key-attribute-long-type">
    <xsd:simpleContent>
      <xsd:extension base="xsd:long">
        <xsd:attribute name="key" type="xsd:string" fixed="key"/>
      </xsd:extension>
    </xsd:simpleContent>
  </xsd:complexType>
"""

let parsed1 = SchemaUtils.head example1 |> Patterns.AsComplexType |> Option.get
parse_complex_type parsed1

((parsed1.ContentModel :?> XmlSchemaSimpleContent).Content :?> XmlSchemaSimpleContentExtension).Attributes

//typedef filename {
//    type string;
//}
// TypeDefStatement (filename,[Type (TypeStatement (string,None))])
let example2 = """
  <xsd:complexType name="filename">
    <xsd:simpleContent>
      <xsd:extension base="xsd:string"/>
    </xsd:simpleContent>
  </xsd:complexType>
"""

let parsed2 = SchemaUtils.head example2 |> Patterns.AsComplexType |> Option.get
parse_complex_type parsed2

//typedef filename {
//    description "Sample documentation";
//    type string;
//}
//TypeDefStatement (filename,
//   [
//    Type (TypeStatement (string,None));
//    Description (DescriptionStatement ("Sample documentation",None))
//   ]
//)
let example2a = """
  <xsd:complexType name="filename">
    <xsd:annotation>
        <xsd:documentation>Sample documentation</xsd:documentation>
    </xsd:annotation>
    <xsd:simpleContent>
      <xsd:extension base="xsd:string"/>
    </xsd:simpleContent>
  </xsd:complexType>
"""
let parsed2a = SchemaUtils.head example2a |> Patterns.AsComplexType |> Option.get
parse_complex_type parsed2a

//leaf source-ipv4-address {
//  description "IP Address to use as source address in IPv4 header appended to intercepted packets";
//  type ipv4addr;
//}
let example3 = """
<xsd:element name="source-ipv4-address" minOccurs="0" type="ipv4addr">
    <xsd:annotation>
    <xsd:documentation>IP Address to use as source address in IPv4 header appended to intercepted packets</xsd:documentation>
    <xsd:appinfo>
        <flag>current-product-support</flag>
    </xsd:appinfo>
    </xsd:annotation>
</xsd:element>
"""

//container interfaces {
//  description "Tunnel Interfaces";
//  status deprecated;
//  uses apply-advanced;
//  list tunnel-interface {
//    key name;
//    description "(null)";
//    uses tunnel_interface_type;
//  }
//}
//
//grouping tunnel_interface_type {
//  description "One or more tunnel interfaces on which to configure flow-tap service";
//  leaf name {
//    description "Tunnel Interface name";
//    junos:must "(!(\"services dynamic-flow-capture\"))";
//    junos:must-message "Dynamic flow capture cannot be configured when flow-tap is configured";
//    junos:must "(!(\"interfaces $$-IFL family inet filter\"))";
//    junos:must-message "Tunnel Interface assigned for Radius-Flow-Tap cannot be configured with firewall filter";
//    junos:must "(\"interfaces $$-IFL family inet\")";
//    junos:must-message "Interface with family inet must be defined in the [edit interfaces] hierarchy";
//    junos:must "(!(\"services dynamic-flow-capture\"))";
//    junos:must-message "Dynamic flow capture cannot be configured when flow-tap is configured";
//    junos:must "(!(\"interfaces $$-IFL family inet filter\"))";
//    junos:must-message "Tunnel Interface assigned for Radius-Flow-Tap cannot be configured with firewall filter";
//    junos:must "(\"interfaces $$-IFL family inet\")";
//    junos:must-message "Interface with family inet must be defined in the [edit interfaces] hierarchy";
//    type interface-unit;
//  }
//  uses apply-advanced;
//}
//
//grouping apply-advanced {
//  description "Apply advanced configuration logic";
//  leaf-list apply-groups {
//    description "Groups from which to inherit configuration data";
//    type string;
//  }
//  leaf-list apply-groups-except {
//    description "Don't inherit configuration data from these groups";
//    type string;
//  }
//}
let example4 = """
<xsd:element name="interfaces" minOccurs="0">
    <xsd:annotation>
        <xsd:documentation>Tunnel Interfaces</xsd:documentation>
        <xsd:appinfo>
            <flag>remove-empty</flag>
            <flag>current-product-support</flag>
            <flag>hidden-from-cli</flag>
            <remove-if-empty/>
            <deprecated>
                <cause>The feature is obsolete</cause>
                <remediation>Discontinue using this knob</remediation>
            </deprecated>
        </xsd:appinfo>
    </xsd:annotation>
    <xsd:complexType>
        <xsd:sequence>
            <xsd:choice minOccurs="0" maxOccurs="unbounded">
            <xsd:element ref="undocumented" minOccurs="0"/>
            <xsd:element ref="junos:comment" minOccurs="0"/>
            <xsd:element name="apply-advanced" minOccurs="0">
                <xsd:annotation>
                    <xsd:documentation>Apply advanced configuration logic</xsd:documentation>
                    <xsd:appinfo>
                        <flag>remove-empty</flag>
                        <flag>master-write</flag>
                        <flag>current-product-support</flag>
                        <remove-if-empty/>
                        <no-activate/>
                    </xsd:appinfo>
                </xsd:annotation>
                <xsd:complexType>
                    <xsd:sequence>
                        <xsd:choice minOccurs="0" maxOccurs="unbounded">
                            <xsd:element ref="undocumented" minOccurs="0"/>
                            <xsd:element ref="junos:comment" minOccurs="0"/>
                        </xsd:choice>
                    </xsd:sequence>
                </xsd:complexType>
            </xsd:element>
            <!-- </apply-advanced> -->

            <xsd:element name="tunnel-interface" minOccurs="0" maxOccurs="unbounded" type="tunnel-interface-type">
                <xsd:annotation>
                    <xsd:appinfo>
                        <flag>homogeneous</flag>
                        <flag>current-product-support</flag>
                    </xsd:appinfo>
                </xsd:annotation>
            </xsd:element>
            <!-- </tunnel-interface> -->
            </xsd:choice>
        </xsd:sequence>
    </xsd:complexType>
</xsd:element>
"""

let example5 = """
<xsd:complexType name="tunnel-interface-type">
  <xsd:annotation>
    <xsd:documentation>One or more tunnel interfaces on which to configure flow-tap service</xsd:documentation>
    <xsd:appinfo>
      <flag>homogeneous</flag>
      <flag>autosort</flag>
      <flag>current-product-support</flag>
    </xsd:appinfo>
  </xsd:annotation>
  <xsd:sequence>
      <xsd:element name="name">
        <xsd:annotation>
          <xsd:documentation>Tunnel Interface name</xsd:documentation>
          <xsd:appinfo>
            <flag>mustquote</flag>
            <flag>identifier</flag>
            <flag>current-product-support</flag>
            <regex-match deprecate="deprecate">^((vt-)|(ip-))</regex-match>
            <regex-match-error deprecate="deprecate">Must be a virtual or ip tunnel interface</regex-match-error>
            <match>
              <pattern>^((vt-)|(ip-))</pattern>
              <message>Must be a virtual or ip tunnel interface</message>
            </match>
              <identifier/>
          </xsd:appinfo>
        </xsd:annotation>
        <xsd:complexType>
          <xsd:simpleContent>
            <xsd:extension base="interface-unit">
              <xsd:attribute name="key" type="xsd:string" fixed="key"/>
            </xsd:extension>
          </xsd:simpleContent>
        </xsd:complexType>
      </xsd:element>
        <!-- </name> -->
      <xsd:choice minOccurs="0" maxOccurs="unbounded">
        <xsd:element ref="undocumented" minOccurs="0"/>
        <xsd:element ref="junos:comment" minOccurs="0"/>
        <xsd:element name="apply-advanced" minOccurs="0">
          <xsd:annotation>
            <xsd:documentation>Apply advanced configuration logic</xsd:documentation>
            <xsd:appinfo>
              <flag>remove-empty</flag>
              <flag>master-write</flag>
              <flag>current-product-support</flag>
                <remove-if-empty/>
              <no-activate/>
            </xsd:appinfo>
          </xsd:annotation>
          <xsd:complexType>
            <xsd:sequence>
              <xsd:choice minOccurs="0" maxOccurs="unbounded">
                <xsd:element ref="undocumented" minOccurs="0"/>
                <xsd:element ref="junos:comment" minOccurs="0"/>
              </xsd:choice>
            </xsd:sequence>
          </xsd:complexType>
        </xsd:element>
        <!-- </apply-advanced> -->
      </xsd:choice>
  </xsd:sequence>
</xsd:complexType>"""

let parsed5 = XmlSchema.Parse (example5, [ junos ])

let example6 = """
<xsd:complexType name="aaa-profile">
  <xsd:annotation>
    <xsd:documentation>AAA profile configuration</xsd:documentation>
    <xsd:appinfo>
      <flag>homogeneous</flag>
      <flag>current-product-support</flag>
    </xsd:appinfo>
  </xsd:annotation>
  <xsd:simpleContent>
    <xsd:extension base="xsd:string"/>
  </xsd:simpleContent>
</xsd:complexType>
"""
let parsed6 = XmlSchema.Parse (example6, [ junos ])
let o6 = SchemaUtils.head example6 :?> XmlSchemaComplexType
let ai6 = o6.Annotation.Items.Item 1 :?> XmlSchemaAppInfo
ai6.Markup |> Array.map (fun v -> v.Name, v.InnerText )



parse_app_info ai6





let input = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "Models-External", "Juniper", "XSD", "juniper-netconf.xsd")
let reader = File.OpenText(input)
let schema = XmlSchema.Read(reader, error_handler)

let item = schema.Items.Item 59 :?> XmlSchemaComplexType
//let item = schema.Items.Item 59 :?> System.Xml.Schema.XmlSchemaElement
item.LineNumber
item.Particle
let annotation = item.Annotation.Items.Item 0 :?> XmlSchemaAppInfo
annotation.Markup.[0].InnerText

