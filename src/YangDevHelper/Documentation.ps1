function New-Notes {
    [CmdletBinding()]
    param(
        [ValidateNotNullOrEmpty()]
        [string]$Filename = "notes",

        [ValidateSet('HTML', 'PDF')]
        [string]$Format = 'HTML'
    )

    if ($rootDir -ne $null) {
        $root = $rootDir
    } else {
        $root = Get-Item -Path $PSScriptRoot |
                Split-Path -Parent |
                Split-Path -Parent
    }
    $directory = Join-Path -Path $root -ChildPath "Documents"
    $executable = Join-Path -Path $root -ChildPath "packages" |
                  Join-Path -ChildPath "Pandoc.Windows" |
                  Join-Path -ChildPath "tools" |
                  Join-Path -ChildPath "Pandoc" |
                  Join-Path -ChildPath "pandoc.exe"

    if (-not (Test-Path -Path $executable -PathType Leaf)) {
        Write-Error -Message "Cannot find pandoc.exe in $executable"
        return
    }

    if (-not (Test-Path -Path $directory -PathType Container)) {
        Write-Error -Message "Cannot find input directory for documents; it is not in: $directory"
        return
    } else {
        Write-Debug -Message "Input directory: $directory"
    }

    $files = Get-ChildItem -Path $directory -Filter ??-*.md |
             ForEach-Object { $_.FullName }
    $files = @((Join-Path -Path $directory -ChildPath "header.md")) + $files

    $arguments = $files + @(
        "--standalone"
        "--number-sections"
    )

    $Filename = $Filename.Trim()
    switch ($Format) {
        "HTML" {
            $arguments += @(
                "-o",
                "$Filename.html"
            )
         }
        "PDF" {
            $arguments += @(
                "-o",
                "$Filename.pdf"
            )
        }
        Default {
            $arguments += @(
                "-o",
                "$Filename.html"
            )
        }
    }

    Write-Debug ("Will run with: {0}" -f ([String]::Join(" ", $arguments)))
    . $executable @arguments
}