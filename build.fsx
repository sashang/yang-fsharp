#r "packages/FAKE/tools/FakeLib.dll"
open System.Diagnostics
open Fake
open Fake.Testing.XUnit2
open System.IO

// TODO: Strategy for compiling and testing
// The following is a bit messy. We compile both debug and release versions.
// We do unit testing of the debug versions. QuickBuild builds release.

//
// Properties
//
let buildDir = Path.Combine(__SOURCE_DIRECTORY__, "build")
let testDir = Path.Combine(__SOURCE_DIRECTORY__, "test")
let testDirResults = Path.Combine(__SOURCE_DIRECTORY__, "test")

let packagesDir = Path.Combine(__SOURCE_DIRECTORY__, "packages")

let exampleTypesDirectory = Path.Combine(__SOURCE_DIRECTORY__, "src", "TypeTests")

//
// Targets
//
Target "Clean" (fun _ ->
    CleanDir buildDir
    CleanDir testDir
)

Target "Build" (fun _ ->
    !! "src/YANG/YANG.sln"
      |> MSBuildRelease buildDir "Build"
      |> Log "AppBuild-Output:"
)

Target "QuickBuild" (fun _ ->
    !! "src/YANG/YANG.sln"
      |> MSBuildRelease buildDir "Build"
      |> Log "AppBuild-Output:"
)

Target "BuildTest" (fun _ ->
    !! "src/**/*.Tests.*.fsproj"
      |> MSBuildDebug testDir "Build"
      |> Log "TestBuild-Output:"
)

Target "GenerateTypes" (fun _ ->
  let dependencies =
    [
      @"System.Reflection.Metadata\lib\portable-net45+win8\System.Reflection.Metadata.dll"
    ]
    |> List.map(fun lib ->
      Path.Combine(packagesDir, lib)
    )

  Directory.GetFiles(exampleTypesDirectory, "*.fsx")
  |> List.ofArray
  |> List.map (fun filename ->
    let outputFile = Path.ChangeExtension(Path.Combine(testDir, filename), ".dll")
    TraceHelper.logVerbosefn "Compiling example type in %s" filename
    let result =
      FscHelper.compile [
        FscHelper.Out         outputFile
        FscHelper.Target      FscHelper.TargetType.Library
        FscHelper.Platform    FscHelper.PlatformType.AnyCpu
        // FscHelper.References  dependencies
        FscHelper.Debug       true
      ] [ filename ]

    if result = 0 then
      filename
    else
      traceError (sprintf "Error compiling example type in file: %s" filename)
      raise <| BuildException(sprintf "Fsc: compile failed for %s with exit code" filename, [ string result ])
  )
  |> ignore
)

Target "Test" (fun _ ->
    !! (testDir + "/*.Tests.xunit.dll")
      |> xUnit2 (fun p ->
        {
            p with
                HtmlOutputPath = Some (Path.Combine(testDirResults, "Parser.tests.result.html"))
                XmlOutputPath = Some (Path.Combine(testDirResults, "Parser.tests.result.xml"))
        }
      )
)

Target "Default" (fun _ ->
    trace "Start building YANG parser and type generator"
)

//
// Dependencies
//
"Clean"
  ==> "Build"
  ==> "BuildTest"
  ==> "Test"
  ==> "Default"

"Build"
  ==> "QuickBuild"

RunTargetOrDefault "Default"
