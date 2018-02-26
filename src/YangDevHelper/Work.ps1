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
    )

    if ($IncludeLowPriority) {
        $identifiers += @(
            "DIVERGENCE"
        )
    }

    $labels = $identifiers | ForEach-Object -Process {
        "// {0}:" -f $_
    }

    Write-Debug -Message "Searching in $RootDir"
    [System.IO.FileInfo[]]$source = Get-ChildItem -Path $RootDir -Include *.fs -Recurse
    if (($source -eq $null) -or ($source.Length -eq 0)) {
        Write-Warning -Message "Did not find any input files"
        return
    }

    $work = $source | ForEach-Object -Process {
        $filename = $_.Name
        Write-Progress -Activity "Parsing" -Status "$filename"

        if ([String]::IsNullOrWhiteSpace($_.DirectoryName)) {
            Write-Warning -Message "Cannot get the directory name for $_"
            $location = ""
        } else {
            $location = $_.DirectoryName.Substring($root.FullName.Length).Trim('\', 1)
        }

        $_ | Select-String -Pattern $labels -AllMatches
    }

    if ($DoNotFormat) {
        $work
    } else {
        $work | ForEach-Object -Process {
            $message = $_.Line.Trim()

            $work = [regex]::Match($message, "// (?<label>[A-z]+): (?<comment>.*)")
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

