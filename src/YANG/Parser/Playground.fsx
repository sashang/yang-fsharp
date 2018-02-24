// Playground.fsx
// Testing of parser functionality during development.

#load @"../../../.paket/load/net471/FParsec-Big-Data-Edition.fsx"
#load @"../../../.paket/load/net471/NLog.fsx"

#load "Utilities.fs"
#load "Errors.fs"
#load "Comments.fs"
#load "Tokens.fs"
#load "Strings.fs"
#load "Namespace.fs"
#load "Generic.fs"
#load "Identifier.fs"
#load "Statements.fs"
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

let simple_model = """
module example-system {
    yang-version 1.1;
    namespace "urn:example:system";
    prefix "sys";

    organization "Example Inc.";
    contact "joe@example.com";
    description
        "The module for entities implementing the Example system.";
}
"""

let sm = run Module.parse_module simple_model

let ``revision info with no description`` = """
revision 2007-06-09;
"""

run (spaces >>. Revisions.parse_revision) ``revision info with no description``

let ``simple revision info with description`` = """
revision 2007-06-09 {
        description "Initial revision.";
    }
"""

run (spaces >>. Revisions.parse_revision) ``simple revision info with description``

let ``revision info with description with options`` = """
revision 2007-06-09 {
    description "Initial revision." {
        ex:documentation-flag 5;
    }
}
"""

run (spaces >>. Revisions.parse_revision) ``revision info with description with options``
