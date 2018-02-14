[CmdletBinding()]
param()

$Env:ROOTDIR = $PSScriptRoot

if (Get-Command -Name npm -ErrorAction SilentlyContinue) {
    $commitizen_path = Join-Path -Path $PSScriptRoot -ChildPath node_modules | `
                       Join-Path -ChildPath commitizen

    if (-not (Test-Path -Path $commitizen_path)) {
        Write-Verbose -Message "Commitizen does not exist; will install"
        npm install commitizen
    }

    if (Test-Path -Path $commitizen_path) {
        $commitizen_bin = Join-Path -Path $commitizen_path -ChildPath bin
        if ($Env:PATH -notcontains $commitizen_bin) {
            Write-Verbose -Message "Adding commitizen to path"
            $Env:PATH += ";$commitizen_bin"
        }
    }

    if (Get-Command -Name commitizen -ErrorAction SilentlyContinue) {
        Write-Verbose -Message "Commitizen seems to be installed"
        # Not sure whether we need to run the following. I think it is needed once per repo:
        # commitizen init cz-conventional-changelog --save-dev --save-exact
    }

} else {
    Write-Warning -Message "node.js is not installed; this is not a problem, but when present gives better options for structuring git commit messages"
}