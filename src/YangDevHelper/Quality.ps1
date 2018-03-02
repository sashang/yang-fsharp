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

    [string[]]$xunit_arguments = @(
        "-target:$xUnitTool",
        "-targetdir:$BuildDirectory",
        '-register:"user"',
        "-mergeoutput",
        "-output:$output"#,
        #"-filter:-[*.Tests]*"
    )

    [string[]]$xunit_arguments_local = @(
        "-target:$xUnitTool",
        '-register:"user"',
        "-mergeoutput",
        "-output:$output"#,
        #"-filter:-[*.Tests]*"
    )

    [string[]]$mstest_arguments = @(
        "-target:mstest",
        "-targetdir:$BuildDirectory",
        '-register:"user"',
        "-mergeoutput",
        "-output:$output"#,
        #"-filter:-[*.Tests]*"
    )

    [string[]]$fsharp_arguments = @(
        "-target:fsc",
        '-register:"user"',
        "-mergeoutput",
        "-output:$output"#,
    )

    Get-ChildItem -Path $BuildDirectory -Filter *.xunit.dll | ForEach-Object -Process {
        $ut = $_.FullName
        Write-Verbose -Message "Processing tests from: $ut"

        [string[]]$arguments = $xunit_arguments + @(
            "-targetargs:$ut"
        )

        Write-Verbose -Message "Processing args: $arguments"

        . "$codeCoverTool" @arguments
    }

    Get-ChildItem -Path $exampleTypesDir -Filter *.fsx | ForEach-Object -Process {
        $ut = $_.FullName
        Write-Verbose -Message "Compiling type from: $ut"

        $args = "{0} -d:DEBUG --target:library" -f $ut
        [string[]]$arguments = $fsharp_arguments + @(
            "-targetdir:$exampleTypesDir",
            "-targetargs:'$args'"
        )

        . "$codeCoverTool" @arguments
    }

    Get-ChildItem -Path $exampleTypesDir -Filter *.dll | ForEach-Object -Process {
        $ut = [System.IO.Path]::Combine($BuildDirectory, $_.Name)
        Write-Verbose -Message "Processing tests from: $ut"

        [string[]]$arguments = $xunit_arguments_local + @(
            "-targetdir:$exampleTypesDir",
            "-targetargs:$ut"
        )

        Write-Verbose -Message "Processing args: $arguments"

        . "$codeCoverTool" @arguments
    }

    [string[]]$extra_unit_tests = @(
        "TypeTestsFromCSharp.dll"
    )

    $extra_unit_tests | ForEach-Object -Process {
        $ut = [System.IO.Path]::Combine($BuildDirectory, $_.Name)
        Write-Verbose -Message "Processing tests from: $ut"

        [string[]]$arguments = $mstest_arguments + @(
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