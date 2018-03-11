// Initialize.fsx
// Helper script to load all project files and dependencies

#load @"../../../.paket/load/net471/NLog.fsx"
#load @"../../../.paket/load/net471/FParsec-Big-Data-Edition.fsx"

#r @"../Model/bin/Debug/Yang.Model.dll"

// Initialize logging
#load "Logging.fsx"

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

[<AutoOpen>]
module MyEnvironment =
    open System
    open System.IO
    open Yang.Parser

    let sample_dir = Path.Combine(__SOURCE_DIRECTORY__, @"../Examples/")
    let external_modules_dir = Path.Combine(__SOURCE_DIRECTORY__, @"../../../", @"Models-External")

    let models_incorrect = [
        // This file seems to be more of a template; it contains statements with missing information
        // but with instructions of how to fix them.
        "YangModels\vendor\cisco\nx\7.0-3-I6-1\cisco-nx-openconfig-if-ip-deviations.yang"
    ]

    let ignore_known_incorrect_models (filename : string) =
        models_incorrect
        |> List.tryFind ( fun v -> filename.Contains(v) )
        |> Option.isNone

    let get_sample_model (filename : string) =
        let full_path = Path.Combine(sample_dir, filename)
        ReadAndClean full_path

    let get_external_model (filename : string) =
        let full_path = Path.Combine(external_modules_dir, filename)
        ReadAndClean full_path

    let get_all_external_models = lazy (
        Directory.EnumerateFiles(external_modules_dir, "*.yang", SearchOption.AllDirectories)
    )

    let fold_on_all_models<'T> (filter : string -> bool) (initial : unit -> 'T) (apply : 'T -> string -> 'T) =
        get_all_external_models.Value
        |> Seq.filter filter
        |> Seq.fold (
            fun state filename ->
                printfn "%A\t\tParsing: %s" (DateTime.Now) (filename.Substring(external_modules_dir.Length))

                try
                    apply state filename
                with
                    | ex ->
                        printfn "Error parsing: %s\n%A" filename ex
                        state

        ) (initial ())

    do
        if Directory.Exists(sample_dir) = false then
            failwith (sprintf "Sample directory does not exist; expected in %s" sample_dir)

        if Directory.Exists(external_modules_dir) = false then
            failwith (sprintf "External modules directory does not exist; expected in %s" external_modules_dir)
