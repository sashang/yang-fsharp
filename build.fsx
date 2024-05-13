#r "paket: groupref netcorebuild //"
#load "./.fake/build.fsx/intellisense.fsx"

open System.Diagnostics
open System.IO
open System.Text
open Fake.Core
open Fake.Core.ld.TargetOperators
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.DotNet.Testing.XUnit2


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

let buildMode = Environment.environVarOrDefault "buildMode" "Release"

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

  let info = FileInfo(filename)
  let outputFile = Path.ChangeExtension(filename, "dll")

  // TODO: Do not compile fsx if output already exists and is recent

  let args = StringBuilder(@"--target:library")

  if debug then
    args.Append(" -d:DEBUG") |> ignore

  Shell.pushd info.DirectoryName

  let result =
    let p = new Process()
    p.StartInfo.FileName <- "fsc"
    p.StartInfo.WorkingDirectory <- info.DirectoryName
    p.StartInfo.Arguments <- sprintf @"%s %s" (args.ToString()) filename

    if not (p.Start()) then
      Trace.traceError (sprintf "Cannot start F# compiler on %s" filename)
      raise <| MSBuildException(sprintf "Fsc: compiler failed to start for %s" filename, [ ])

    let timeout = System.TimeSpan.FromMinutes 5.0
    p.WaitForExit(timeout.TotalMilliseconds |> int) |> ignore
    p.ExitCode

  if result <> 0 then
    Trace.traceError (sprintf "Error compiling example type in file: %s (error: %d)" filename result)
    raise <| MSBuildException(sprintf "Fsc: compile failed for %s with exit code" filename, [ string result ])

  if debug then
    Shell.copyFile testDir outputFile
  else
    Shell.copyFile buildDir outputFile

  Shell.popd ()
  filename

let setDebugParams (defaults : MSBuildParams) =
  { defaults with
      Verbosity   = Some(Quiet)
      Targets     = ["Build"]
      Properties  =
        [
          "Optimize", "False"
          "DebugSymbols", "True"
          "Configuration", "Debug"
        ]
  }

let setReleaseParams (defaults : MSBuildParams) =
  { defaults with
      Verbosity   = Some(Quiet)
      Targets     = ["Build"]
      Properties  =
        [
          "Optimize", "False"
          "DebugSymbols", "True"
          "Configuration", "Release"
        ]
  }

let setParams (defaults : MSBuildParams) =
  { defaults with
      Verbosity   = Some(Quiet)
      Targets     = ["Build"]
      Properties  =
        [
          "Optimize", "False"
          "DebugSymbols", "True"
          "Configuration", buildMode
        ]
  }

//
// Targets
//
Target.create "Clean" (fun _ ->
    // Clean release and test directories
    Shell.cleanDir buildDir
    Shell.cleanDir testDir
    Shell.cleanDir testDirResults

    // Delete generated types
    Directory.GetFiles(exampleTypesDirectory, "*.dll") |> File.deleteAll
)

Target.create "Build" (fun _ ->
    !! "src/YANG/YANG.sln"
      |> Fake.DotNet.MSBuild.runRelease id buildDir "Build"
      |> Trace.logItems "AppBuild-Output:"
)

Target.create "BuildDebug" (fun _ ->
    !! "src/YANG/YANG.sln"
      |> Fake.DotNet.MSBuild.runDebug id testDir "Build"
      |> Trace.logItems "AppBuild-Output:"
)

Target.create "QuickBuild" (fun _ ->
    !! "src/YANG/YANG.sln"
      |> Fake.DotNet.MSBuild.runRelease id buildDir "Build"
      |> Trace.logItems "AppBuild-Output:"
)

// We want to build all unit tests, and the type generator.
Target.create "BuildTest" (fun _ ->
    !! "src/**/*.Tests.*.fsproj"
      |> Fake.DotNet.MSBuild.runDebug id testDir "Build"
      |> Trace.logItems "TestBuild-Output:"
)

Target.create "Test" (fun _ ->
    !! (testDir + "/*.Tests.xunit.dll")
      |> Testing.XUnit2.run (fun p ->
        {
            p with
                HtmlOutputPath = Some (Path.Combine(testDirResults, "YANG.tests.result.html"))
                XmlOutputPath = Some (Path.Combine(testDirResults, "YANG.tests.result.xml"))
        }
      )
)

Target.create "BuildDebugTypeGenerator" ( fun _ ->
    !! "src/YANG/Generator/Generator.fsproj"
      |> Fake.DotNet.MSBuild.runDebug id testDir "Build"
      |> Trace.logItems "TestBuild-Type Generator Output:"
)

Target.create "GenerateTypesForTesting" (fun _ ->
  Directory.GetFiles(exampleTypesDirectory, "*.fsx")
  |> List.ofArray
  |> List.iter (fun filename ->
    [ CompileFsx (filename, false) ] |> Trace.logItems  (sprintf "Compiling (debug)   : %s\n" filename)
    [ CompileFsx (filename, true)  ] |> Trace.logItems  (sprintf "Compiling (release) : %s\n" filename)
  )
)

Target.create "BuildCSharpTests" (fun _ ->
  !! "src/TypeTestsFromCSharp/TypeTestsFromCSharp.sln"
  |> Fake.DotNet.MSBuild.runDebug id testDir "Build"
  |> Trace.logItems "TestBuild-Build C# unit tests"
)

Target.create "TestGeneratedTypes" (fun _ ->
  let fsharpTests =
    Directory.GetFiles(exampleTypesDirectory, "*.fsx")
    |> Seq.map (fun filename ->
      let fi = FileInfo(filename)
      let basename = fi.Name
      let outname = Path.Combine(testDir, basename)
      Path.ChangeExtension(outname, "dll")
    )

  fsharpTests
    |> Testing.XUnit2.run (fun p ->
      {
          p with
              HtmlOutputPath = Some (Path.Combine(testDirResults, "Generator.tests.result.html"))
              XmlOutputPath = Some (Path.Combine(testDirResults, "Generator.tests.result.xml"))
      }
    )
)

Target.create "TestCSharpTypes" (fun _ ->
  let csharpReportDir = Path.Combine(testDirResults, "CSharpUnitTests")
  Shell.mkdir csharpReportDir
  !! Path.Combine(testDir, "Yang.Examples.CSharp.Tests.dll")
    |> Testing.MSTest.exec (fun p ->
      { p with
          // TODO: Can we get the MSTest.exe from nuget.org?
          ToolPath = Path.Combine(System.Environment.GetEnvironmentVariable("DevEnvDir"), "MSTest.exe")
          ResultsDir = csharpReportDir
      })
)

Target.create "Default" (fun _ ->
    Trace.log "Start building YANG parser and type generator"
)

Target.create "All" (fun _ ->
    Trace.log "Build and test everything from scratch"
)

//
// Dependencies
//
"Build"
  ==> "BuildTest"
  ==> "Test"
  ==> "BuildDebugTypeGenerator"
  ==> "GenerateTypesForTesting"
  ==> "BuildCSharpTests"
  ==> "TestCSharpTypes"
  ==> "Default"
  ==> "TestGeneratedTypes"

"GenerateTypesForTesting" ==> "BuildDebug"
"Clean" ==> "All"
"TestGeneratedTypes" ==> "All"

Target.runOrDefaultWithArguments "Default"
