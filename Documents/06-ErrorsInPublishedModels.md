# Errors in published models

Some of the published YANG models which are stored in `Models-External`
contain error. Most typically, the errors relate to the use of backslash in strings
and in regular expressions (`pattern` statements). In particular, for regular expressions,
often it is the case that the RE string is copied as is without the extra escaping of the
RE escape character. Recall that YANG strings accept their own escape characters, and
that set is very limited.

Another common mistake is the use of the backslash to end a line in multi-line strings
(similar to C). This is not required in YANG, and it is in fact an error.

There are patches for the models in `Models-External\Patches`.

There are also a few other problems.

## An approach for quickly finding and fixing the errors

Use powershell and add the following commands to your shell:

```PowerShell
function Replace-DoubleBackSlash ($filename=$Global:filename) { $v = Get-Content $filename; $v -replace "\\","\\" | Set-Content $filename -Encoding Ascii }
function Remove-FinalEscape ($filename=$Global:filename) { $v = Get-Content $filename; $v -replace "\s*\\$","" | Set-Content $filename -Encoding Ascii }
function inspect($filename=$Global:filename) { (get-content $filename) -match "\\" }
function check($filename=$Global:filename) { git diff $filename }
function commit($filename=$Global:filename) { git add $filename; $Global:filename = "" }
```

Next, to examine a particular file, set its name (and even path) in `$filename`, and
then run `inspect` to see the list of uses of backslashes. If the problem is one of the usual ones,
use either `Replace-DoubleBackSlash` or `Remove-FinalEscape` to resolve the problem.
Then use `check` to see the changes, and `commit` to add it in the repo.
