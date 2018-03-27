[CmdletBinding()]
param(
    [switch]$WriteToFile,

    [string]$OutputFile = "bad_models.txt"
)

$bad_patterns = @(
    "revision YYYY-MM-DD",
    "deviate /* add, replace, or delete */ {"
)

$files_with_errors = `
    Get-ChildItem . -Include *.yang -Recurse |
    Where-Object -FilterScript {
        Select-String -InputObject $_ -Quiet -SimpleMatch -Pattern $bad_patterns
    }

if ($WriteToFile) {
    $current_path = $pwd.ProviderPath
    $files_with_errors | ForEach-Object -Process { $_.FullName.Replace($current_path, "").TrimStart('/','\') } | Sort-Object -Unique | Out-File $OutputFile -Encoding ascii -Force
} else {
    $files_with_errors
}