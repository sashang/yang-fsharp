﻿// Playground.fsx
// Testing of parser functionality during development.

#load "Initialize.fsx"

open FParsec
open Yang.Parser
open Yang.Model.Statements

open Initialize
open Logging
open Yang.Model.Identifier

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

// File:Models-External\Juniper\17.2\17.2R1\junos\configuration.yang        Line:86639, 86672, 86705, 86738, 89183
// File:Models-External\Juniper\17.2\17.2R1\junos-es\configuration.yang     Line:61494
// File:Models-External\Juniper\17.2\17.2R1\junos-ex\configuration.yang     Line:44820
// File:Models-External\Juniper\17.2\17.2R1\junos-qfx\configuration.yang    Line:47896
// File:Models-External\Juniper\17.3\17.3R1\junos\configuration.yang        Line:98101, 98134, 98167, 98200, 100879
// File:Models-External\Juniper\17.3\17.3R1\junos-es\configuration.yang     Line:68797
// File:Models-External\Juniper\17.3\17.3R1\junos-ex\configuration.yang     Line:49429
// File:Models-External\Juniper\17.3\17.3R1\junos-qfx\configuration.yang    Line:53134
// File:Models-External\Juniper\17.4\17.4R1\junos\conf\junos-conf-class-of-service@2017-01-01.yang      Line: 1168, 1201, 1234


let configuration = """container configuration {
     uses juniper-config;
     list groups {
     }
   }"""

apply_parser BodyStatements.parse_container_statement configuration
apply_parser BodyStatements.parse_body_statement configuration


let input = "'1 .. 128'"
apply_parser Arguments.parse_length input

let input = """length "1 .. 128";"""
apply_parser parse_length_statement input

let unknown1 = """junos:posix-pattern "^.{1,64}$";"""
apply_parser parse_unknown_statement unknown1

apply_parser Types.parse_type_statement input

