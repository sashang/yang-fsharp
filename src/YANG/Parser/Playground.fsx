// Playground.fsx
// Testing of parser functionality during development.

#load @"../../../.paket/load/net471/FParsec-Big-Data-Edition.fsx"
#load @"../../../.paket/load/net471/NLog.fsx"

#load "Utilities.fs"
#load "Errors.fs"
#load "Comments.fs"
#load "Tokens.fs"
#load "Strings.fs"
#load "Generic.fs"
#load "Identifier.fs"
#load "Statements.fs"
#load "Header.fs"
#load "Meta.fs"
#load "Revisions.fs"
#load "Module.fs"
#load "Parser.fs"

open System.IO
open System.Text
open FParsec
open Yang.Parser

let sample_dir = Path.Combine(__SOURCE_DIRECTORY__, @"../Examples/")
Directory.Exists(sample_dir)

let example = Path.Combine(sample_dir, @"RFC7950/example-system.yang")

let ReadAndClean (filename : string) =
    use reader = new StreamReader(filename)
    let sb = StringBuilder()
    use writer = new StringWriter(sb)
    Comments.Remove(reader, writer)
    sb.ToString()

let model = ReadAndClean example

run Module.parse_module model

#time
let big_model =
    Path.Combine(__SOURCE_DIRECTORY__, @"../../../", @"Models-External\Juniper\16.1\configuration.yang")
    |> ReadAndClean

let apply parser input =
    match (run parser input) with
    | Success (result, _, _)    -> result
    | Failure (message, _, _)   -> failwith "parsing failed"

let juniper = apply Generic.parse_many_statements big_model |> List.head
juniper.Keyword
juniper.Argument
juniper.Body
#time

let simple_body = """yang-version 1.1;
namespace "urn:example:system";
prefix "sys";

organization "Example Inc.";
contact "joe@example.com";
description
    "The module for entities implementing the Example system.";

revision 2007-06-09 {
    description "Initial revision.";
}
"""

run (Header.parse_header .>>. Meta.parse_meta .>>. Revisions.parse_revision_list) simple_body

let simple_model = """
module example-system {
    yang-version 1.1;
    namespace "urn:example:system";
    prefix "sys";

    organization "Example Inc.";
    contact "joe@example.com";
    description
        "The module for entities implementing the Example system.";

    revision 2007-06-09 {
        description "Initial revision.";
    }
}
"""

let sm = run Module.parse_module simple_model

let header = """yang-version 1.1;
    namespace "urn:example:system";
    prefix "sys";
"""

run (spaces >>. Header.parse_header) header


let empty_meta = ""
run (spaces >>. Meta.parse_meta) empty_meta

let meta = """organization "Example Inc.";
    contact "joe@example.com";
    description
        "The module for entities implementing the Example system.";
"""

run (spaces >>. Meta.parse_meta) meta
