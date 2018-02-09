#r "packages/FAKE/tools/FakeLib.dll"
open Fake
open System.IO

//
// Properties
//
let buildDir = Path.Combine(__SOURCE_DIRECTORY__, "build")

//
// Targets
//
Target "Clean" (fun _ ->
    CleanDir buildDir
)

Target "Build" (fun _ ->
    !! "src/YANG/YANG.sln"
      |> MSBuildRelease buildDir "Build"
      |> Log "AppBuild-Output:"
)

Target "Default" (fun _ ->
    trace "Start building YANG parser and type generator"
)

//
// Dependencies
//
"Clean"
  ==> "Build"
  ==> "Default"

RunTargetOrDefault "Default"
