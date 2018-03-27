// Playground.fsx
// Testing of parser functionality during development.

#load "Initialize.fsx"

open FParsec
open Yang.Model
open Yang.Parser
open Yang.Parser.BodyStatements

open Initialize
open Logging

MyLog.myLog.AddTrace(Header._name)
MyLog.myLog.AddTrace(Types._name)
MyLog.myLog.AddTrace(Statements._name)
MyLog.myLog.AddTrace(GenericParser._name)
MyLog.myLog.AddTrace(Deviation._name)

//
// Test code to parse files
//
let model = get_sample_model @"RFC7950/example-system.yang"
apply_parser Module.parse_module model

let big_model = get_external_model @"Juniper\16.1\configuration.yang"

// This is what we want to parse eventually

let juniper = apply_parser (spaces >>. Module.parse_module) big_model

#time
// Typical statistics: Real: 00:00:14.234, CPU: 00:00:14.203, GC gen0: 1231, gen1: 1160, gen2: 0
let juniper_def_use = Yang.Model.DefUseResolver.VisitDefinitions (fun _ -> true) (Yang.Model.Statements.Module juniper)

printfn "Length: %d" juniper_def_use.Length

let filex = @"e:\temp\failed.yang"
let file1 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\YangModels\vendor\cisco\xe\1631\ned.yang"
let file2 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\YangModels\standard\mef\test\example\packages\mef\src\yang\mef.yang"
let file5 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\YangModels\experimental\ietf-extracted-YANG-modules\ietf-voucher-request@2018-02-14.yang"
let file6 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\YangModels\vendor\cisco\nx\7.0-3-I7-3\cisco-nx-openconfig-bgp-deviations.yang"

//  revision YYYY-MM-DD {
//../../../Models-External\YangModels\standard\mef\src\model\draft\common\mef-types.yang
//../../../Models-External\YangModels\standard\mef\src\model\draft\mef-legato-global.yang
//../../../Models-External\YangModels\standard\mef\src\model\draft\mef-legato-interfaces.yang
//../../../Models-External\YangModels\standard\mef\src\model\draft\mef-legato-services.yang

//System.Exception: a parser created with createParserForwardedToRef was not initialized
//../../../Models-External\YangModels\standard\mef\test\example\packages\mef\src\yang\mef.yang
//../../../Models-External\YangModels\vendor\cisco\xe\1631\MIBS\SNMP-FRAMEWORK-MIB-ann.yang

//    deviate  {
//../../../Models-External\YangModels\vendor\cisco\nx\7.0-3-I6-1\cisco-nx-openconfig-if-ip-deviations.yang
//../../../Models-External\YangModels\vendor\cisco\nx\7.0-3-I6-1\cisco-nx-openconfig-routing-policy-deviations.yang

//        tailf:dependency ".";
//../../../Models-External\YangModels\vendor\cisco\xe\1631\ned.yang

Parser.ParseFile filex
Parser.ParseFile file1
Parser.ParseFile file2
Parser.ParseFile file2
Parser.ParseFile file5
Parser.ParseFile file6

Parser.ReadAndClean filex

let input = """tailf:transaction-hook "subtree" {
          tailf:invocation-mode "per-transaction";
        }"""
apply_parser parse_unknown_statement input

let input = """tailf:callpoint "ncs-rfs-service-hook" {
        tailf:transaction-hook "subtree" {
          tailf:invocation-mode "per-transaction";
        }
      }"""
apply_parser parse_unknown_statement input
apply_parser generic_yang_statement_implementation input

let input = """list ethernet-virtual-connection {
      tailf:callpoint "ncs-rfs-service-hook" {
        tailf:transaction-hook "subtree" {
          tailf:invocation-mode "per-transaction";
        }
      }
    }"""
apply_parser parse_list_statement input

let input = """container mef-evc-service {  
    list ethernet-virtual-connection {
      tailf:callpoint "ncs-rfs-service-hook" {
        tailf:transaction-hook "subtree" {
          tailf:invocation-mode "per-transaction";
        }
      }
    }
  }
}"""
apply_parser parse_container_statement input


let input = """module mef {
  namespace "http://tail-f.com/mef-evc-service";
  prefix mef-evc-service;

  container mef-evc-service {  
    list ethernet-virtual-connection {
      tailf:callpoint "ncs-rfs-service-hook" {
        tailf:transaction-hook "subtree" {
          tailf:invocation-mode "per-transaction";
        }
      }
    }
  }
}"""
apply_parser Module.parse_module input


let input = """must "count(*) > 1" {
        tailf:dependency ".";
      }"""
apply_parser parse_must_statement input

let input = """rc:yang-data voucher-request-artifact {
    uses voucher-request-grouping;
  }"""
apply_parser parse_unknown_statement input

let input = """augment "/if:interfaces/if:interface" {
    when "if:type = 'ianaif:ieee8023adLag' or
          if:type = 'ianaif:ethernetCsmacd' or 
          if:type = 'ianaif:bridge'" {
      description
        "Applies to Ethernet interfaces or Bridge Ports.";
      }}"""
apply_parser parse_augment_statement input

let input = """output {
          leaf success {
            type boolean;
            mandatory true;
        	description
          	  "Did the action succeed?";
          }
          leaf message {
            type string;
            mandatory true;
        	description
          	  "Any message associated with the output.";
          }}"""
apply_parser parse_output_statement input

let input = "oc-bgp:/bgp/peer-groups/peer-group/oc-bgp-mp:use-multiple-paths/oc-bgp-mp:config/oc-bgp-mp:enabled"
let transform_invalid_absolute_path (input : string) =
    if input.StartsWith "/" then input
    elif input.Contains(":/") then
        // input does not start with '/', but maybe we need to transform it to make it valid
        sprintf "/%s" (input.Replace(":/", ":"))
    else
        // parsing will fail; let it fail later
        input
transform_invalid_absolute_path input

apply_parser Identifier.parse_schema_node_identifier_absolute input
apply_parser Identifier.parse_schema_node_identifier_absolute (transform_invalid_absolute_path input)

let input = """deviation oc-bgp:/bgp/peer-groups/peer-group/oc-bgp-mp:use-multiple-paths/oc-bgp-mp:config/oc-bgp-mp:enabled {
    deviate "not-supported"; 
  }"""

let input = """deviation /oc-bgp:bgp/peer-groups/peer-group/oc-bgp-mp:use-multiple-paths/oc-bgp-mp:config/oc-bgp-mp:enabled {
    deviate "not-supported"; 
  }"""

let input = """deviation /oc-bgp:bgp/peer-groups/peer-group/oc-bgp-mp:use-multiple-paths/oc-bgp-mp:config/oc-bgp-mp:enabled {
    deviate not-supported;
  }"""

apply_parser parse_deviation_statement input

// TODO: error message is not descriptive
apply_parser parse_body_statement input

let input = """deviate "not-supported";"""
apply_parser parse_deviate_not_supported_statement input

let input = """deviate "add";"""
apply_parser parse_deviate_add_statement input


let input = """unique "input/state input/symbol";"""
apply_parser parse_unique_statement input

let input = "input/state input/symbol"
apply_parser Identifier.parse_unique input
apply_parser (sepBy1 Identifier.parse_schema_node_identifier spaces) input

apply_parser (sepBy1 Identifier.parse_schema_node_identifier spaces1) "input/state"

apply_parser (sepBy1 Identifier.parse_schema_node_identifier spaces1) "input/state input/symbol"
apply_parser (sepBy1 Identifier.parse_schema_node_identifier spaces1) "input/state  input/symbol"
apply_parser (sepBy1 Identifier.parse_schema_node_identifier spaces1) "input/state\tinput/symbol"
apply_parser (sepBy1 Identifier.parse_schema_node_identifier spaces1) "input/state\ninput/symbol"

apply_parser (sepBy1 Identifier.parse_schema_node_identifier spaces1) "input/state input/symbol input/symbol/e2"

apply_parser (Identifier.parse_schema_node_identifier .>> spaces .>>. Identifier.parse_schema_node_identifier) input

let input = "input/state"
apply_parser Identifier.parse_schema_node_identifier input
apply_parser (sepBy Identifier.parse_identifier_reference (skipChar '/')) input


let input = """config:required-identity sal:dom-data-store;"""
apply_parser (     (parse_config_statement  |>> Statement.Config)
               <|> (parse_unknown_statement |>> Statement.Unknown)) input

let input = """smiv2:alias "ciscoQosPIBMIB" {
    smiv2:oid "1.3.6.1.4.1.9.18.2.1";
  }"""

apply_parser parse_statement input

(*
How do we parse examples from p.169?
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:services/ex:ssh"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:user[ex:name=fred]"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:user[ex:name='fred']"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:server[ex:ip='192.0.2.1'][ex:port='80']"
apply_parser Identifier.parse_schema_node_identifier 
*)

open FParsec
open Yang.Parser.BodyStatements

// TODO: should give 1 .. 96
apply_parser Arguments.parse_range_part "1..96"

open Yang.Parser.Types
let type1 = """type string7only;"""
apply_parser parse_type_statement type1

let type2 = """type string-huge;"""
apply_parser parse_type_statement type2

let type3 = """type "assoc-type";"""
apply_parser parse_type_statement type3

let type4 = """type assoc-type;"""
apply_parser parse_type_statement type4

let type5 = """type "string";"""
apply_parser parse_type_statement type5

let type6 = """type string;"""
apply_parser parse_type_statement type6




let input = """module ieee802-dot1ax {
  //
  augment "/if:interfaces/if:interface" {
    when "if:type = 'ianaif:ieee8023adLag'"
  }
}"""

let output = Comments.Remove input
output.Split([| '\n' |]).Length = 5
