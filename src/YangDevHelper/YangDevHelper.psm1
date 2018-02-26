$rootDir = Join-Path -Path $PSScriptRoot -ChildPath .. | Join-Path -Child ..
$outputDir = Join-Path -Path $rootDir -ChildPath "build"
$packageDir = Join-Path -Path $rootDir -ChildPath packages

$xUnitTool = Join-Path -Path $packageDir -ChildPath "xunit.runner.console" |
             Join-Path -ChildPath "tools" |
             Join-Path -ChildPath "net452" |
             Join-Path -ChildPath "xunit.console.exe"

$codeCoverTool = Join-Path -Path $packageDir -ChildPath "OpenCover" |
                 Join-Path -ChildPath "tools" |
                 Join-Path -ChildPath "OpenCover.Console.exe"

$reportTool = Join-Path -Path $packageDir -ChildPath "ReportGenerator" |
              Join-Path -ChildPath "tools" |
              Join-Path -ChildPath "ReportGenerator.exe"

$reportDirectory = Join-Path -Path $rootDir -ChildPath "Reports"
$codeCoverReportDirectory = Join-Path -Path $reportDirectory -ChildPath "CodeCoverage"

if (-not (Test-Path $xUnitTool -PathType Leaf)) {
    Write-Warning -Message "Cannot find xUnit tool: $xUnitTool"
}

if (-not (Test-Path $codeCoverTool -PathType Leaf)) {
    Write-Warning -Message "Cannot find code coverage tool: $codeCoverTool"
}
if (-not (Test-Path $reportTool -PathType Leaf)) {
    Write-Warning -Message "Cannot find report tool: $reportTool"
}

. $PSScriptRoot/Work.ps1
. $PSScriptRoot/Quality.ps1
