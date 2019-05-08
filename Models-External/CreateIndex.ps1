[CmdletBinding()]
param()

if (-not(Get-Command -Name "Get-YangUniqueModels" -ErrorAction SilentlyContinue)) {
    $path = Join-Path -Path $PSScriptRoot -ChildPath .. | `
            Join-Path -ChildPath src
    Push-Location -Path $path
    Import-Module .\YangDevHelper
    Pop-Location
}

$outfile = Join-Path -Path $PSScriptRoot -ChildPath "all_yang_models.txt"
Get-YangUniqueModels -Path $PSScriptRoot -OutFile $outfile
