#r "packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Testing.XUnit2
open System.IO

//
// Properties
//
let buildDir = Path.Combine(__SOURCE_DIRECTORY__, "build")
let testDir = Path.Combine(__SOURCE_DIRECTORY__, "test")
let testDirResults = Path.Combine(__SOURCE_DIRECTORY__, "test")

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

Target "BuildTest" (fun _ ->
    !! "src/**/*.Tests.*.fsproj"
      |> MSBuildDebug testDir "Build"
      |> Log "TestBuild-Output:"
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

RunTargetOrDefault "Default"
