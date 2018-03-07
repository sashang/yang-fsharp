// Playground.fsx
// Testing of parser functionality during development.

#load @"../../../.paket/load/net471/FParsec-Big-Data-Edition.fsx"
#load @"../../../.paket/load/net471/NLog.fsx"
#r @"../Model/bin/Debug/Yang.Model.dll"


[<AutoOpen>]
module MyLog =
    open NLog

    type MyLog internal () =
        let config = new Config.LoggingConfiguration()
        let target = new Targets.ConsoleTarget("fsi")

        do
            config.AddTarget(target)
            LogManager.Configuration <- config

        member this.AddTrace (name : string) =
            let name' = sprintf "*_%s" name
            let rule = new Config.LoggingRule(name', NLog.LogLevel.Trace, target)
            config.LoggingRules.Add(rule)
            LogManager.Configuration <- config

    let myLog = MyLog ()


#load "Utilities.fs"
#load "Errors.fs"
#load "Comments.fs"
#load "Strings.fs"
#load "Arguments.fs"
#load "Generic.fs"
#load "Identifier.fs"
#load "Statements.fs"
#load "Header.fs"
#load "Linkage.fs"
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
apply_parser Module.parse_module model


#time
let big_model =
    Path.Combine(__SOURCE_DIRECTORY__, @"../../../", @"Models-External\Juniper\16.1\configuration.yang")
    |> ReadAndClean

let juniper = apply_parser Generic.parse_many_statements big_model |> List.head
juniper.Keyword
juniper.Argument
let st = juniper.Body.Value
st |> List.map (fun s -> s.Keyword) |> List.groupBy id |> List.map (fun (i, v) -> i, List.length v)

let rec RetrieveKeywords (model : Generic.Statement) : string seq = seq {
    yield model.Keyword

    match model.Body with
    | None      -> ()
    | Some st   ->
        for s in st do
            yield! (RetrieveKeywords s)
}

let keywords = RetrieveKeywords juniper |> Seq.groupBy id |> Seq.map (fun (id, l) -> id, Seq.length l) |> Seq.toList

// This is what we want to parse eventually
myLog.AddTrace(Header._name)
let juniper' = apply_parser Module.parse_module big_model












let configuration = """container configuration {
     uses juniper-config;
     list groups {
     }
   }"""

apply_parser BodyStatements.parse_container_statement configuration
apply_parser BodyStatements.parse_body_statement configuration

#time
