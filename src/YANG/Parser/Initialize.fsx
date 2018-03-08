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
    open System.IO
    open Yang.Parser

    let sample_dir = Path.Combine(__SOURCE_DIRECTORY__, @"../Examples/")
    let external_modules_dir = Path.Combine(__SOURCE_DIRECTORY__, @"../../../", @"Models-External")

    let get_sample_model (filename : string) =
        let full_path = Path.Combine(sample_dir, filename)
        ReadAndClean full_path

    let get_external_model (filename : string) =
        let full_path = Path.Combine(external_modules_dir, filename)
        ReadAndClean full_path

    let get_all_external_models = lazy (
        Directory.EnumerateFiles(external_modules_dir, "*.yang", SearchOption.AllDirectories)
    )

    do
        if Directory.Exists(sample_dir) = false then
            failwith (sprintf "Sample directory does not exist; expected in %s" sample_dir)

        if Directory.Exists(external_modules_dir) = false then
            failwith (sprintf "External modules directory does not exist; expected in %s" external_modules_dir)
