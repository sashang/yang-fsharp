#r "packages/FAKE/tools/FakeLib.dll"
open Fake.MSTest
open System.Diagnostics
open Fake
open Fake.Testing.XUnit2
open System.IO
open System.Text

// TODO: Strategy for compiling and testing
// The following is a bit messy. We compile both debug and release versions.
// We do unit testing of the debug versions. QuickBuild builds release.

// TODO: We should bring as a package a good F# compiler and set up build system to use that.

//
// Properties
//
let buildDir = Path.Combine(__SOURCE_DIRECTORY__, "build")
let testDir = Path.Combine(__SOURCE_DIRECTORY__, "test")
let testDirResults = Path.Combine(__SOURCE_DIRECTORY__, "Reports")

let packagesDir = Path.Combine(__SOURCE_DIRECTORY__, "packages")

let exampleTypesDirectory = Path.Combine(__SOURCE_DIRECTORY__, "src", "TypeTests")

//
// Helper scripts
//

/// The following compiles a script (typically using the type provider) and writes to output directory
let CompileFsx (filename : string, debug : bool) =
  // TODO: Compile with the Fake FscHelper target.
  // let outputFile = Path.ChangeExtension(Path.Combine(testDir, filename), ".dll")
  // TraceHelper.logVerbosefn "Compiling example type in %s" filename
  // let result =
  //   FscHelper.compile [
  //     FscHelper.Out         outputFile
  //     FscHelper.Target      FscHelper.TargetType.Library
  //     FscHelper.Platform    FscHelper.PlatformType.AnyCpu
  //     // FscHelper.References  dependencies
  //     FscHelper.Debug       true
  //   ] [ filename ]

  let outputFile = Path.ChangeExtension(filename, "dll")

  // TODO: Do not compile fsx if output already exists and is recent

  let args = StringBuilder(@"--target:library")

  if debug then
    args.Append(" -d:DEBUG") |> ignore

  let result = ExecProcess (fun info ->
    info.FileName <- "fsc"
    info.WorkingDirectory <- exampleTypesDirectory
    info.Arguments <- sprintf @"%s %s" (args.ToString()) filename) (System.TimeSpan.FromMinutes 5.0)

  if result <> 0 then
    traceError (sprintf "Error compiling example type in file: %s" filename)
    raise <| BuildException(sprintf "Fsc: compile failed for %s with exit code" filename, [ string result ])

  if debug then
    Fake.FileHelper.CopyFile testDir outputFile
  else
    Fake.FileHelper.CopyFile buildDir outputFile

  filename

//
// Targets
//
Target "Clean" (fun _ ->
    // Clean release and test directories
    CleanDir buildDir
    CleanDir testDir
    CleanDir testDirResults

    // Delete generated types
    Directory.GetFiles(exampleTypesDirectory, "*.dll") |> DeleteFiles
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

// We want to build all unit tests, and the type generator.
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
                HtmlOutputPath = Some (Path.Combine(testDirResults, "YANG.tests.result.html"))
                XmlOutputPath = Some (Path.Combine(testDirResults, "YANG.tests.result.xml"))
        }
      )
)

Target "BuildDebugTypeGenerator" ( fun _ ->
    !! "src/YANG/Generator/Generator.fsproj"
      |> MSBuildDebug testDir "Build"
      |> Log "TestBuild-Type Generator Output:"
)

Target "GenerateTypesForTesting" (fun _ ->
  Directory.GetFiles(exampleTypesDirectory, "*.fsx")
  |> List.ofArray
  |> List.iter (fun filename ->
    [ CompileFsx (filename, false) ] |> Log (sprintf "Compiling (debug)   : %s" filename)
    [ CompileFsx (filename, true)  ] |> Log (sprintf "Compiling (release) : %s" filename)
  )
)

Target "BuildCSharpTests" (fun _ ->
  !! "src/TypeTestsFromCSharp/TypeTestsFromCSharp.sln"
  |> MSBuildDebug testDir "Build"
  |> Log "TestBuild-Build C# unit tests"
)

Target "TestGeneratedTypes" (fun _ ->
  let fsharpTests =
    Directory.GetFiles(exampleTypesDirectory, "*.fsx")
    |> Seq.map (fun filename ->
      let fi = FileInfo(filename)
      let basename = fi.Name
      let outname = Path.Combine(testDir, basename)
      Path.ChangeExtension(outname, "dll")
    )

  fsharpTests
    |> xUnit2 (fun p ->
      {
          p with
              HtmlOutputPath = Some (Path.Combine(testDirResults, "Generator.tests.result.html"))
              XmlOutputPath = Some (Path.Combine(testDirResults, "Generator.tests.result.xml"))
      }
    )
)

Target "TestCSharpTypes" (fun _ ->
  let csharpReportDir = Path.Combine(testDirResults, "CSharpUnitTests")
  FileUtils.mkdir csharpReportDir
  !! Path.Combine(testDir, "Yang.Examples.CSharp.Tests.dll")
    |> MSTest (fun p ->
      { p with
          // TODO: Can we get the MSTest.exe from nuget.org?
          ToolPath = Path.Combine(System.Environment.GetEnvironmentVariable("DevEnvDir"), "MSTest.exe")
          ResultsDir = csharpReportDir
      })
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
  ==> "GenerateTypesForTesting"
  ==> "BuildCSharpTests"
  ==> "TestCSharpTypes"
  ==> "Default"

"BuildDebugTypeGenerator"
  ==> "GenerateTypesForTesting"
  ==> "TestGeneratedTypes"
  ==> "TestCSharpTypes"

RunTargetOrDefault "Default"
