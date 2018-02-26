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
#load "Types.fs"
#load "Leaf.fs"
#load "Module.fs"
#load "Parser.fs"

open System.IO
open System.Text
open FParsec
open Yang.Parser

let apply parser input =
    match (run parser input) with
    | Success (result, _, _)    -> result
    | Failure (message, _, _)   -> failwith "parsing failed"

let definition_body = """leaf host-name {
    type string;
    description
        "Hostname for this system.";
}"""

let definition = apply Leaf.parse_leaf definition_body 
definition.Identifier
definition.Type
definition.Description


//
// Test code to parse files
//

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

run Module.parse_module ""
run Module.parse_module model

#time
let big_model =
    Path.Combine(__SOURCE_DIRECTORY__, @"../../../", @"Models-External\Juniper\16.1\configuration.yang")
    |> ReadAndClean



let juniper = apply Generic.parse_many_statements big_model |> List.head
juniper.Keyword
juniper.Argument
juniper.Body
#time

