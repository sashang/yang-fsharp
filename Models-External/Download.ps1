[CmdletBinding(SupportsShouldProcess=$True)]
param(
    [switch]$Update
)

[PSCustomObject[]]$repositories = @(
    [PSCustomObject]@{ Name = "BroadbandForum"; Link = "https://github.com/BroadbandForum/yang.git" },
    [PSCustomObject]@{ Name = "Juniper";        Link = "https://github.com/Juniper/yang.git" },
    [PSCustomObject]@{ Name = "networkop";      Link = "https://github.com/networkop/yang.git" },
    [PSCustomObject]@{ Name = "openconfig";     Link = "https://github.com/openconfig/public.git" },
    [PSCustomObject]@{ Name = "sysrepo";        Link = "https://github.com/sysrepo/sysrepo.git" },
    [PSCustomObject]@{ Name = "tail-f-systems"; Link = "https://github.com/tail-f-systems/JNC.git" },
    [PSCustomObject]@{ Name = "YangModels";     Link = "https://github.com/YangModels/yang.git" }
)

Push-Location -Path $PSScriptRoot

foreach($repo in $repositories) {
    $name = $repo.Name

    Write-Verbose -Message "Checking $name"

    if (-not (Test-Path -Path $name -PathType Container)) {
        Write-Verbose -Message "Cloning $name"

        if($PSCmdlet.ShouldProcess("$name", "Cloning (git clone)")) {
            git clone --recurse-submodule $repo.Link $name
        }
    } else {
        Write-Verbose -Message "Repo $name is present"

        if ($Update) {
            if ($PSCmdlet.ShouldProcess("$name", "Updating (git pull)")) {
                Push-Location -Path $name
                git pull
                Pop-Location
            }
        }
    }
}

Pop-Location