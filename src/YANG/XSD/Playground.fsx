#r "System.Xml.dll"
#r "System.Xml.Linq.dll"

open System
open System.IO
open System.Text
open FSharp.Collections
open System.Xml.Schema

type Namespaces = {
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

// Define your library scripting code here
let input = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "Models-External", "Juniper", "XSD", "juniper-netconf.xsd")
let reader = File.OpenText(input)

let error_handler (_ : obj) (args : ValidationEventArgs) =
    printfn "Error (%A) in line (%d,%d) : %s" args.Severity args.Exception.LineNumber args.Exception.LinePosition args.Message

let schema = XmlSchema.Read(reader, error_handler)

let parse_xml_schema (schema : string, prefix : string, extra : Namespaces list) =
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

let example1 = """
  <xsd:complexType name="key-attribute-long-type">
    <xsd:simpleContent>
      <xsd:extension base="xsd:long">
        <xsd:attribute name="key" type="xsd:string" fixed="key"/>
      </xsd:extension>
    </xsd:simpleContent>
  </xsd:complexType>
"""

//typedef filename {
//    type string;
//}
let example2 = """
  <xsd:complexType name="filename">
    <xsd:simpleContent>
      <xsd:extension base="xsd:string"/>
    </xsd:simpleContent>
  </xsd:complexType>
"""

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

let junos = Namespaces.Make ("junos", "http://xml.juniper.net/junos/15.1X24/junos", "junos.xsd")
let xx = parse_xml_schema (example5, "xsd", [ junos ])
let items : XmlSchemaObject list = xx.Items |> Seq.cast |> Seq.toList
let item = items.Head :?> XmlSchemaComplexType
//let item = items.Head :?> XmlSchemaElement
item.Name
item.Annotation
cc.BaseTypeName.Name
