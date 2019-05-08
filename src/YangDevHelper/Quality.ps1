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

    # REFACTOR: Refactor code below to separate functions, and document
    # FIX: Code below does not cover code in the type creation scripts. Why not?

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
        "-output:$output",
        "-filter:`"+[Yang.*]* -[*.Tests.*]*`""
    )

    [string[]]$xunit_arguments_local = @(
        "-target:$xUnitTool",
        '-register:"user"',
        "-mergeoutput",
        "-output:$output",
        "-filter:`"+[Yang.*]* -[*.Tests.*]*`""
    )

    [string[]]$mstest_arguments = @(
        "-target:$msTestTool",
        "-targetdir:$BuildDirectory",
        '-register:"user"',
        "-mergeoutput",
        "-output:$output",
        "-filter:`"+[Yang.*]* -[*.Tests.*]*`""
    )

    [string[]]$fsharp_arguments = @(
        "-target:$fsc",
        '-register:"user"',
        "-mergeoutput",
        "-output:$output",
        "-filter:`"+[Yang.*]* -[Yang.*]ProviderImplementation.* -[*.Tests.*]*`""
    )

    # Run normal unit tests
    Get-ChildItem -Path $BuildDirectory -Filter *.xunit.dll | ForEach-Object -Process {
        $ut = $_.FullName
        Write-Verbose -Message "Processing tests from: $ut"

        [string[]]$arguments = $xunit_arguments + @(
            "-targetargs:$ut"
        )

        Write-Verbose -Message "Processing args: $arguments"

        . "$codeCoverTool" @arguments
    }

    # Compile type provider examples
    Get-ChildItem -Path $exampleTypesDir -Filter *.fsx | ForEach-Object -Process {
        $ut = $_.FullName
        Write-Verbose -Message "Compiling type from: $ut"

        $args = "{0} -d:DEBUG --target:library " -f $ut
        [string[]]$arguments = $fsharp_arguments + @(
            "-targetdir:$exampleTypesDir",
            "-targetargs:`"$args`""
        )

        Try {
            Push-Location -Path $_.DirectoryName
            . "$codeCoverTool" @arguments
        }
        Finally {
            Pop-Location
        }
    }

    # Run the tests in the type provider examples
    Get-ChildItem -Path $exampleTypesDir -Filter *.dll |
    Where-Object -Property Name -NotMatch "FSharp.*.dll" |
    ForEach-Object -Process {
        $ut = [System.IO.Path]::Combine($BuildDirectory, $_.Name)
        Write-Verbose -Message "Processing tests from: $ut"

        [string[]]$arguments = $xunit_arguments_local + @(
            "-targetdir:$exampleTypesDir",
            "-targetargs:$ut"
        )

        Write-Verbose -Message "Processing args: $arguments"

        . "$codeCoverTool" @arguments
    }

    # Run the integration with C# tests
    [string[]]$extra_unit_tests = @(
        "Yang.Examples.CSharp.Tests.dll"
    )

    $extra_unit_tests | ForEach-Object -Process {
        $ut = [System.IO.Path]::Combine($BuildDirectory, $_)
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
}

function Show-Cloc {
    [CmdletBinding()]
    param()

    if (-not (Get-Command -Name cloc -ErrorAction SilentlyContinue)) {
        Write-Error -Message "Cannot find 'cloc' utility; exiting"
        return
    }

    $excluded = "`"{0}`"" -f (Join-Path -Path $PSScriptRoot -ChildPath "ExcludedFilesForCloc.txt")
    Push-Location -Path $rootDir
    cloc --exclude-dir=packages,build,test,paket-files,TestResults,Models-External,.paket,node_modules --exclude-ext=xml --exclude-list-file=$excluded .
    Pop-Location
}