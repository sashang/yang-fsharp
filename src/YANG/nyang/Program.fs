module Yang.Nyang

open System
open System.IO
open Yang.Model
open Yang.Parser
open Yang.Parser.Module

let fail_with_help () =
    Printf.eprintfn "Error in command"
    Environment.Exit(-1)
    failwith "This point should not be reached"

let validate arguments =
    match arguments with
    | []    ->
        Printf.eprintfn "Validate takes at least one argument"
        fail_with_help ()
    | model :: _ ->
        try
            match Parser.ParseFile model with
            | ModelUnit.ModuleUnit m ->
                printfn "Detected module: %s" m.Name.Value
                printfn "%s" (Printer.ModuleToString m)
            | ModelUnit.SubmoduleUnit m ->
                printfn "Detected submodule: %s" m.Name.Value

        with
        | :? YangParserException ->
            Printf.eprintfn "Error in parsing model"
            Environment.Exit(-1)
            ()

[<EntryPoint>]
let main argv =
    Parser.Initialize()

    let all_args = Environment.GetCommandLineArgs()
    let command = all_args.[0]
    let file_with_extension = FileInfo(command)
    let file = file_with_extension.Name.Replace(file_with_extension.Extension, "")

    let operation, args =
        if file.Equals("nyang", StringComparison.InvariantCultureIgnoreCase) then
            if argv.Length = 0 then
                fail_with_help ()
            else
                argv.[0], (Array.toList argv |> List.tail)

        else file, (Array.toList argv)

    if operation.Equals("validate", StringComparison.InvariantCultureIgnoreCase) then
        validate args
    else
        Printf.eprintfn "Unknown operation: %s" operation
        fail_with_help ()

    0 // return an integer exit code
