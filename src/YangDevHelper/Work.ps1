function Get-WorkItems {
    [CmdletBinding()]
    param(
        [ValidateNotNullOrEmpty()]
        [ValidateScript({ Test-Path -Path $_ -PathType Container })]
        [string]$RootDir = (Join-Path -Path $PSScriptRoot -ChildPath ".."),

        [switch]$IncludeLowPriority,
        [switch]$DoNotFormat
    )

    $root = Get-Item -Path $RootDir
    if ([String]::IsNullOrWhiteSpace($root.FullName)) {
        Write-Error -Message "Cannot get the full name of root: $root"
    }

    $identifiers = @(
        "TODO"
        "HACK"
        "FIX"
        "BUG"
    )

    if ($IncludeLowPriority) {
        $identifiers += @(
            "DIVERGENCE"
            "REFACTOR"
        )
    }

    $labels = $identifiers | ForEach-Object -Process {
        "(//|#) {0}:" -f $_
    }

    Write-Debug -Message "Searching in $RootDir"
    [System.IO.FileInfo[]]$source = Get-ChildItem -Path $RootDir -Include *.fs,*.fsi,*.fsx,*.cs,*.ps1 -Recurse
    if (($source -eq $null) -or ($source.Length -eq 0)) {
        Write-Warning -Message "Did not find any input files"
        return
    }
    $source += @(
        Get-Item -Path ([System.IO.Path]::Combine($RootDir, '..', 'build.fsx'))
        Get-Item -Path ([System.IO.Path]::Combine($RootDir, '..', 'init.ps1'))

    )

    $work = $source | ForEach-Object -Process {
        $filename = $_.Name
        Write-Progress -Activity "Parsing" -Status "$filename"

        $_ | Select-String -Pattern $labels -AllMatches
    }

    if ($DoNotFormat) {
        $work
    } else {
        $work | ForEach-Object -Process {
            $message = $_.Line.Trim()

            $work = [regex]::Match($message, "(//|#) (?<label>[A-z]+): (?<comment>.*)")
            Write-Verbose $work

            [PSCustomObject]@{
                Filename = $_.Filename
                Line     = $_.LineNumber
                Label    = $work.Groups["label"].Value
                Message  = $work.Groups["comment"].Value
            }
        }
    }
}

