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












let configuration = """container configuration {
     uses juniper-config;
     list groups {
     }
   }"""

apply_parser BodyStatements.parse_container_statement configuration
apply_parser BodyStatements.parse_body_statement configuration

#time
