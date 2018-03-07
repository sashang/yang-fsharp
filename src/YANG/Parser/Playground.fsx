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

let rec RetrieveKeywords (model : Generic.Statement) : string seq = seq {
    yield model.Keyword

    match model.Body with
    | None      -> ()
    | Some st   ->
        for s in st do
            yield! (RetrieveKeywords s)
}

let keywords = RetrieveKeywords juniper |> Seq.groupBy id |> Seq.map (fun (id, l) -> id, Seq.length l) |> Seq.toList

type YangPathItem = string * (string option)
type YangPath = YangPathItem list

let FindAllNodes (filter : Generic.Statement -> bool) (model : Generic.Statement) =
    let rec find (path : YangPath) (node : Generic.Statement) : YangPath list =
        let label = (node.Keyword, node.Argument) :: path
        let current = if filter node then [ label ] else []
        let children =
            match node.Body with
            | None -> []
            | Some children -> children |> List.collect (fun c -> find label c)

        children @ current

    let result = find [] model
    result |> List.map (fun r -> List.rev r)

let yang_filter_by_keyword_and_argument (keyword, argument) (node : Generic.Statement) =
    node.Keyword.Equals(keyword) && (node.Argument = argument)

let PrintPath (path : YangPath) =
    path
    |> List.map (
        fun (keyword, argument) ->
            match argument with
            | Some arg  -> sprintf "%s %s" keyword arg
            | None      -> sprintf "%s" keyword
    )
    |> String.concat " -> "

FindAllNodes (yang_filter_by_keyword_and_argument ("container", Some "interfaces")) juniper |> List.map PrintPath |> List.iter (printfn "%A")
FindAllNodes (yang_filter_by_keyword_and_argument ("container", Some "traceoptions")) juniper |> List.map PrintPath |> List.iter (printfn "%A")
FindAllNodes (yang_filter_by_keyword_and_argument ("choice", Some "vlan_tag_mode")) juniper |> List.map PrintPath |> List.iter (printfn "%A")
FindAllNodes (yang_filter_by_keyword_and_argument ("leaf", Some "vlan-id")) juniper |> List.map PrintPath |> List.iter (printfn "%A")
FindAllNodes (yang_filter_by_keyword_and_argument ("container", Some "family")) juniper |> List.map PrintPath |> List.iter (printfn "%A")
FindAllNodes (yang_filter_by_keyword_and_argument ("container", Some "inet6")) juniper |> List.map PrintPath |> List.iteri (fun v -> printfn "%A" v)


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
