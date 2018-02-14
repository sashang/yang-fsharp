// Yang.Parser.performance
// This is a helper program that will be used for performance testing of the parser.

open System
open System.IO
open System.Text
open Yang.Parser

let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    returnValue, timer.ElapsedMilliseconds

let remove_comments (input : string) =
    duration (
        fun () ->
            let input = new StreamReader(input)
            let sb = StringBuilder()
            let output = new StringWriter(sb)
            Comments.Remove (input, output)
            sb.ToString()
    )

let models_subdirectory = "Models-External"

let root_dir =
#if INTERACTIVE
#else
    let from_environment = Environment.GetEnvironmentVariable("ROOTDIR")
    if String.IsNullOrWhiteSpace(from_environment) then
        // the user has not provided a path in the environment
        if Directory.Exists(models_subdirectory) then "."
        elif Directory.Exists(Path.Combine("..", models_subdirectory)) then ".."
        elif Directory.Exists(Path.Combine("..", "..", models_subdirectory)) then
            Path.Combine("..", "..")
        elif Directory.Exists(Path.Combine("..", "..", "..", models_subdirectory)) then
            Path.Combine("..", "..", "..")
        elif Directory.Exists(Path.Combine("..", "..", "..", "..", models_subdirectory)) then
            Path.Combine("..", "..", "..", "..")
        elif Directory.Exists(Path.Combine("..", "..", "..", "..", "..", models_subdirectory)) then
            Path.Combine("..", "..", "..", "..", "..")
        else
            failwith "Cannot find root of solution"
    else
        from_environment
#endif

[<EntryPoint>]
let main argv =
    let input = @"Models-External\Juniper\16.1\configuration.yang"
    let _, time = remove_comments (Path.Combine(root_dir, input))
    printfn "Elapsed Time: %i ms" time
    0 // return an integer exit code
