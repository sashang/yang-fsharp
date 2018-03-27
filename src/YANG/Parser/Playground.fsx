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

  // TODO: test with augment-stmt and uses-augment-stmt

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

(*
How do we parse examples from p.169?
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:services/ex:ssh"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:user[ex:name=fred]"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:user[ex:name='fred']"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:server[ex:ip='192.0.2.1'][ex:port='80']"
apply_parser Identifier.parse_schema_node_identifier 
*)

let xx = apply_parser parse_path "/ex:system/ex:server[ex:ip='192.0.2.1'][ex:port='80']/ex:other"
xx._Schema.[1].IsValid

// TODO: should give 1 .. 96
apply_parser Arguments.parse_range_part "1..96"

