module DotnetYang.Main

open System
open System.IO
open Yang.Model
open Yang.Parser
open Yang.Parser.Module
open Argu
open DotnetYang.Generate
open Fantomas.Core
open Fabulous.AST

let validate filenames =

    let rec helper remaining acc =
        match remaining with
        | []  ->
            acc
        | h :: t ->
            try
                let result = ParseFile h
                helper t (Ok(result) :: acc)
            with
            | :? YangParserException ->
                helper t (Error(h) :: acc)

    helper filenames []

type CliArguments =
    | Output of string
    | [<MainCommand;ExactlyOnce;Last>] Files of string list

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Output _ -> "Name of the output assembly"
            | Files _ -> "List of files to be processed"

[<EntryPoint>]
let main argv =
    Parser.Initialize()
    let cliParser = ArgumentParser.Create<CliArguments>(programName = "dotnet-yang")
    let results = cliParser.Parse(argv)
    if results.Contains(Output) then
        //parse and generate output
        let results = validate (results.GetResult(Files))
        for result in results do
            match result with
            | Ok model ->
                let oak = generateModel model
                oak
                |> Gen.mkOak
                |> CodeFormatter.FormatOakAsync
                |> Async.RunSynchronously
                |> printfn "%s"
            | Error filename -> printfn "%s: Error" filename
        ()
    else
        // parse only
        let results = validate (results.GetResult(Files))
        for result in results do
            match result with
            | Ok model -> ()
            | Error filename -> eprintfn "%s: Error" filename

    0 // return an integer exit code
