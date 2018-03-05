// Playground.fsx
// Testing of parser functionality during development.

#r @"..\..\..\packages\FSharp.Core\lib\net45\FSharp.Core.dll"

#load @"../../../.paket/load/net471/FParsec-Big-Data-Edition.fsx"
#load @"../../../.paket/load/net471/NLog.fsx"
#r @"../Model/bin/Debug/Yang.Model.dll"

#load "Utilities.fs"
#load "Errors.fs"
#load "Comments.fs"
#load "Arguments.fs"
#load "Strings.fs"
#load "Generic.fs"
#load "Identifier.fs"
#load "Statements.fs"
#load "Header.fs"
#load "Meta.fs"
#load "Revisions.fs"
#load "Types.fs"
#load "Leaf.fs"
#load "BodyStatements.fs"
#load "Module.fs"
#load "Parser.fs"

open System.IO
open System.Text
open FParsec
open Yang.Parser
open System.Data
open Yang.Model.Statements

//
// Test code to parse files
//

let sample_dir = Path.Combine(__SOURCE_DIRECTORY__, @"../Examples/")
Directory.Exists(sample_dir)

let example = Path.Combine(sample_dir, @"RFC7950/example-system.yang")

let model = ReadAndClean example

let xxx = apply_parser Module.parse_module model

let body = """container system {
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
    }"""

apply_parser BodyStatements.parse_body_statement body

let leaf = """leaf full-name {
    type string;
}"""

apply_parser Leaf.parse_leaf leaf

let leaf_body = "type string;"
apply_parser Leaf.parse_leaf_body leaf_body
let yy = apply_parser Types.parse_type leaf_body
yy.GetType()
let zz = LeafBodyStatement.Make.Do yy
let zzz = LeafBodyStatement.Description ("fff", None)
apply_parser (Types.parse_type |>> (fun v -> LeafBodyStatement.Type v)) leaf_body

LeafBodyStatement.Description ("help", None)


#time
let big_model =
    Path.Combine(__SOURCE_DIRECTORY__, @"../../../", @"Models-External\Juniper\16.1\configuration.yang")
    |> ReadAndClean

let juniper = apply_parser Generic.parse_many_statements big_model |> List.head
juniper.Keyword
juniper.Argument
juniper.Body

// This is what we want to parse eventually
let juniper' = apply_parser Module.parse_module big_model
#time

