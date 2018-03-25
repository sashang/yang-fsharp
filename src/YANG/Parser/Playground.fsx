// Playground.fsx
// Testing of parser functionality during development.

#load "Initialize.fsx"

open FParsec
open Yang.Parser
open Yang.Parser.BodyStatements

open Initialize
open Logging

MyLog.myLog.AddTrace(Header._name)
MyLog.myLog.AddTrace(Types._name)
MyLog.myLog.AddTrace(Statements._name)
MyLog.myLog.AddTrace(GenericParser._name)

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


let file1 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\YangModels\vendor\cisco\xr\631\openconfig-vlan-types.yang"
let file3 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\YangModels\vendor\cisco\xr\612\Cisco-IOS-XR-infra-policymgr-cfg.yang"
let file4 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\draft\interface\bbf-fiber-types.yang"

Statements.generic_parser_generator.IsSome
Statements.generic_parser_implementations
GenericParser.initialize ()

Parser.ParseFile file1
Parser.ParseFile file3
Parser.ParseFile file4

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
