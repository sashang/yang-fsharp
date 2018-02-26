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
#load "DataDefinitions.fs"
#load "Body.fs"
#load "Module.fs"
#load "Parser.fs"

open System.IO
open System.Text
open FParsec
open Yang.Parser

let body = """
container system {
    leaf host-name {
        type string;
        description
            "Hostname for this system.";
    }

    leaf-list domain-search {
        type string;
        description
            "List of domain names to search.";
    }

    container login {
        leaf message {
            type string;
            description
            "Message given at start of login session.";
        }

        list user {
            key "name";
            leaf name {
                type string;
            }
            leaf full-name {
                type string;
            }
            leaf class {
                type string;
            }
        }
    }
}
"""

let m = apply_parser (spaces >>. DataDefinitions.parse_data_definition) body


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



let juniper = apply_parser Generic.parse_many_statements big_model |> List.head
juniper.Keyword
juniper.Argument
juniper.Body
#time

