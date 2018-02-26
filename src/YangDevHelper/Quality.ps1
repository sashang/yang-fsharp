function Show-CodeCoverage {
    [CmdletBinding()]
    param(
        [ValidateNotNullOrEmpty()]
        [string]$Directory = $codeCoverReportDirectory,

        [ValidateNotNullOrEmpty()]
        [ValidateScript({Test-Path -Path $_ -PathType Container})]
        [string]$BuildDirectory = $outputDir,

        [ValidateNotNullOrEmpty()]
        [string]$OutputFile = "results.xml"
    )

    Write-Verbose -Message "Report will be generated in: $Directory"
    if (Test-Path -Path $Directory -PathType Container) {
        Write-Verbose -Message "Emptying target directory: $Directory"
        Remove-Item -Path $Directory -Recurse -Force
    }

    Write-Verbose -Message "Creating output folder: $Directory"
    New-Item -Path $Directory -ItemType Container -Force

    if ($OutputFile.Contains('/')) {
        $output = $OutputFile
    } else {
        $output = Join-Path -Path $Directory -ChildPath $OutputFile
    }

    [string[]]$all_arguments = @(
        "-target:$xUnitTool",
        "-targetdir:$BuildDirectory",
        '-register:"user"',
        "-mergeoutput",
        "-output:$output"#,
        #"-filter:-[*.Tests]*"
    )

    Get-ChildItem -Path $BuildDirectory -Filter *.xunit.dll | ForEach-Object -Process {
        $ut = $_.FullName
        Write-Verbose -Message "Processing tests from: $ut"

        [string[]]$arguments = $all_arguments + @(
            "-targetargs:$ut"
        )

        Write-Verbose -Message "Processing args: $arguments"

        . "$codeCoverTool" @arguments
    }

    [string[]]$report_arguments = @(
        "-reports:$output",
        "-reporttypes:Html",
        "-targetdir:$Directory"
    )
    . "$reportTool" @report_arguments

    $index_page = Join-Path -Path $Directory -ChildPath "index.htm"
    if (-not (Test-Path -Path $index_page -PathType Leaf)) {
        Write-Error -Message "Cannot find generated page in: $index_page"
    } else {
        . $index_page
    }

    #  .\packages\OpenCover\tools\OpenCover.Console.exe  -targetargs:"
        # $p\build\Yang.Parser.Tests.xunit.dll" -targetdir:"$p\build" -register:"user"
    #  .\packages\ReportGenerator\tools\ReportGenerator.exe -reports:.\results.xml  -reporttypes:Html
}