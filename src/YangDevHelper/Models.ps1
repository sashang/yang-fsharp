function Get-UniqueModels {
    [CmdletBinding()]
    param(
        $Path = $PWD,

        [string]$OutFile = $null
    )

    $properPath = Get-Item $Path
    $properPath = $properPath.FullName

    Write-Debug -Message "Searching in $Path"
    $existing = New-Object -Type 'System.Collections.Generic.HashSet[string]'

    if (-not [String]::IsNullOrWhiteSpace($OutFile)) {
        if (Test-Path -Path $OutFile) {
            Write-Warning -Message "Removing old file: $OutFile"
            Remove-Item -Path $OutFile -Force
        }
    }

    Get-ChildItem -Path $Path -Include *.yang -Recurse | `
    ForEach-Object -Process {
        $filename = $_.FullName
        # The following seems to be more robust for files that contains '[]'
        # than calling Get-FileHash directly.
        $filename2 = $filename.Replace("[", "``[").Replace("]", "``]")
        $checksum = Get-FileHash -Path $filename2
        if ($checksum -eq $null) {
            Write-Warning -Message "Cannot compute checksum for $filename"
        }

        if (-not (Get-Member -InputObject $checksum -name "Hash" -MemberType Properties)) {
            Write-Warning -Message "[Skipping file] Cannot find hash property for $filename"
        }
        else {
            $key = $checksum.Hash
            if ($existing.Contains($key)) {
                Write-Verbose -Message "File $filename already exists"
            } else {
                # File does not exist

                if ([String]::IsNullOrWhiteSpace($OutFile)) {
                    $filename
                } else {
                    $filename = $filename.Replace($properPath, "")
                    $filename = $filename.TrimStart(@('\', '/'))
                    $filename | Out-File -Append -FilePath $OutFile -Encoding ascii
                }

                $existing.Add($key) | Out-Null
            }
        }
    }
}