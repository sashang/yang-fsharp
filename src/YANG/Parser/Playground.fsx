// Playground.fsx
// Testing of parser functionality during development.

#load "Initialize.fsx"

open FParsec
open Yang.Parser

open Initialize
open Logging

//
// Test code to parse files
//
let model = get_sample_model @"RFC7950/example-system.yang"
apply_parser Module.parse_module model

let big_model = get_external_model @"Juniper\16.1\configuration.yang"

// This is what we want to parse eventually
MyLog.myLog.AddTrace(Header._name)
MyLog.myLog.AddTrace(Types._name)
let juniper = apply_parser (spaces >>. Module.parse_module) big_model

#time
// Typical statistics: Real: 00:00:14.234, CPU: 00:00:14.203, GC gen0: 1231, gen1: 1160, gen2: 0
let juniper_def_use = Yang.Model.DefUseResolver.VisitDefinitions (fun _ -> true) (Yang.Model.Statements.Module juniper)
printfn "Length: %d" juniper_def_use.Length


// File:Models-External\Juniper\17.2\17.2R1\junos\configuration.yang        Line:86639, 86672, 86705, 86738, 89183
// File:Models-External\Juniper\17.2\17.2R1\junos-es\configuration.yang     Line:61494
// File:Models-External\Juniper\17.2\17.2R1\junos-ex\configuration.yang     Line:44820
// File:Models-External\Juniper\17.2\17.2R1\junos-qfx\configuration.yang    Line:47896
// File:Models-External\Juniper\17.3\17.3R1\junos\configuration.yang        Line:98101, 98134, 98167, 98200, 100879
// File:Models-External\Juniper\17.3\17.3R1\junos-es\configuration.yang     Line:68797
// File:Models-External\Juniper\17.3\17.3R1\junos-ex\configuration.yang     Line:49429
// File:Models-External\Juniper\17.3\17.3R1\junos-qfx\configuration.yang    Line:53134
// File:Models-External\Juniper\17.4\17.4R1\junos\conf\junos-conf-class-of-service@2017-01-01.yang      Line: 1168, 1201, 1234


apply_parser (pip Strings.parse_string Identifier.parse_schema_node_identifier_absolute) "'/if:interfaces/if:interface'"

let file1 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\draft\interface\bbf-fiber-base.yang"
let file2 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\draft\interface\bbf-fiber-channelpair-body.yang"
let file3 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\draft\interface\bbf-fiber-if-type.yang"
let file4 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\draft\interface\bbf-fiber-types.yang"
let file5 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\standard\interface\bbf-fast-line-performance-body.yang"
let file6 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\standard\interface\bbf-interfaces-performance-management.yang"
let file7 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\Juniper\16.1\junos-extension.yang"
let file8 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\Juniper\17.4\17.4R1\junos-qfx\rpc\junos-qfx-rpc-clear@2017-01-01.yang"
let file9 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\YangModels\vendor\huawei\network-router\8.9.10\huawei\huawei-qos-hqos.yang"
let file10 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\YangModels\vendor\huawei\network-router\8.9.10\huawei\huawei-system.yang"
let file11 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\YangModels\vendor\cisco\xr\611\Cisco-IOS-XR-infra-alarm-logger-oper.yang"

MyLog.myLog.AddTrace(Statements._name)
MyLog.myLog.AddTrace(GenericParser._name)

Statements.generic_parser_generator.IsSome
Statements.generic_parser_implementations
GenericParser.initialize ()

Parser.ParseFile file1
Parser.ParseFile file2
Parser.ParseFile file3
Parser.ParseFile file4
Parser.ParseFile file5
Parser.ParseFile file6
Parser.ParseFile file7
Parser.ParseFile file8
Parser.ParseFile file9
Parser.ParseFile file10
Parser.ParseFile file11

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


