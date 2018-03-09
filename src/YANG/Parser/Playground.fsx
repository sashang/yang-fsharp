// Playground.fsx
// Testing of parser functionality during development.

#load "Initialize.fsx"

open System.IO
open System.Text
open FParsec
open Yang.Parser
open System.Data
open Yang.Model.Statements

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
let juniper = apply_parser Module.parse_module big_model

// File:Models-External\Juniper\17.2\17.2R1\junos\configuration.yang        Line:86639, 86672, 86705, 86738, 89183
// File:Models-External\Juniper\17.2\17.2R1\junos-es\configuration.yang     Line:61494
// File:Models-External\Juniper\17.2\17.2R1\junos-ex\configuration.yang     Line:44820
// File:Models-External\Juniper\17.2\17.2R1\junos-qfx\configuration.yang    Line:47896
// File:Models-External\Juniper\17.3\17.3R1\junos\configuration.yang        Line:98101, 98134, 98167, 98200, 100879
// File:Models-External\Juniper\17.3\17.3R1\junos-es\configuration.yang     Line:68797
// File:Models-External\Juniper\17.3\17.3R1\junos-ex\configuration.yang     Line:49429
// File:Models-External\Juniper\17.3\17.3R1\junos-qfx\configuration.yang    Line:53134
// File:Models-External\Juniper\17.4\17.4R1\junos\conf\junos-conf-class-of-service@2017-01-01.yang      Line: 1168, 1201, 1234

//  pattern "\*";
let input = "\"\\*\""
let input2 = "\\*"
apply_parser (spaces >>. Strings.parse_unquoted_string) input

apply_parser Generic.statement_parser """pattern "\*";"""
apply_parser Generic.statement_parser """pattern "\\*";"""

// [RFC 7950, p. 150]
let pattern1 = """pattern '[a-zA-Z_][a-zA-Z0-9\-_.]*';"""
let pattern2 = """pattern '[xX][mM][lL].*' {
         modifier invert-match;
       }"""

apply_parser Generic.statement_parser pattern1
apply_parser Generic.statement_parser pattern2



let path1 = """path
        '/dot1q:bridges'+
        '/dot1q:bridge'+
        '/dot1q:component'+
        '/psfp:flow-meters'+
        '/psfp:flow-meter-instance-table'+
        '/psfp:flow-meter-instance-id';"""
apply_parser Generic.statement_parser path1 

let str1 = """
        '/dot1q:bridges'+
        '/dot1q:bridge'+
        '/dot1q:component'+
        '/psfp:flow-meters'+
        '/psfp:flow-meter-instance-table'+
        '/psfp:flow-meter-instance-id'
"""
apply_parser (spaces >>. Strings.parse_string) str1

let str2 = """
        "/dot1q:bridges"+
        "/dot1q:bridge"+
        "/dot1q:component"+
        "/psfp:flow-meters"+
        "/psfp:flow-meter-instance-table"+
        "/psfp:flow-meter-instance-id"
"""
apply_parser (spaces >>. Strings.parse_string) str2




let actual = """
"[a-zA-Z0-9 .,\\\'\\\";:-_/\\!@]*"
"""
let actual' = actual.Trim().Trim('"')

let pattern = """
"[a-zA-Z0-9 .,\\\\\\'\\\\\\\";:-_/\\\\!@]*"
"""
let parsed1 = apply_parser (spaces >>. Strings.parse_string) pattern





let configuration = """container configuration {
     uses juniper-config;
     list groups {
     }
   }"""

apply_parser BodyStatements.parse_container_statement configuration
apply_parser BodyStatements.parse_body_statement configuration


open Generic
let multi_empty = "statement 1; ; statement 2;"
apply_parser Generic.parse_many_statements multi_empty
let multi_empty_2 = "statement 1;; statement 2;"
apply_parser Generic.parse_many_statements multi_empty_2
let multi_empty_3 = "statement 1 {} statement 2;"
apply_parser Generic.parse_many_statements multi_empty_3
let multi_empty_4 = "statement 1 {} ;; statement 2;"
apply_parser Generic.parse_many_statements multi_empty_4
let multi_empty_5 = "statement 1 {;} statement 2;"
apply_parser Generic.parse_many_statements multi_empty_5

#time
