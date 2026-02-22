# run_tests.ps1 - Run Bel test suite and verify results (live output)
# Usage: .\tests\run_tests.ps1 [-BelExe .\target\release\bel.exe] [-BelFile bel.bel]

param(
    [string]$BelExe = ".\target\release\bel.exe",
    [string]$BelFile = "bel.bel",
    [string]$TestFile = ".\tests\test_guide_simple.bel"
)

# Parse test file: extract (expression, expected) pairs
$tests = @()
$lines = Get-Content $TestFile
for ($i = 0; $i -lt $lines.Count; $i++) {
    $line = $lines[$i].Trim()
    # Skip comments and blank lines
    if ($line -eq "" -or $line.StartsWith(";")) { continue }
    # The next non-blank line starting with "; expect:" is the expected result
    for ($j = $i + 1; $j -lt $lines.Count; $j++) {
        if ($lines[$j] -match '^\s*;\s*expect:\s*(.+)$') {
            $tests += [PSCustomObject]@{
                Expr     = $line
                Expected = $Matches[1].Trim()
            }
            break
        }
    }
}

Write-Host "Running $($tests.Count) tests..." -ForegroundColor Cyan
Write-Host "  Interpreter: $BelExe"
Write-Host "  Loading:     $BelFile"
Write-Host "  Test file:   $TestFile"
Write-Host ""

# Start the interpreter process with stdin/stdout pipes
$psi = New-Object System.Diagnostics.ProcessStartInfo
$psi.FileName = (Resolve-Path $BelExe).Path
$psi.Arguments = "--load $BelFile"
$psi.UseShellExecute = $false
$psi.RedirectStandardInput = $true
$psi.RedirectStandardOutput = $true
$psi.RedirectStandardError = $true
$psi.CreateNoWindow = $true

$proc = [System.Diagnostics.Process]::Start($psi)

# Wait for loading to complete by reading stderr
# The interpreter prints "Loading bel.bel..." and " done (N expressions)" to stderr
$loadMsg = ""
while ($true) {
    $ch = $proc.StandardError.Read()
    if ($ch -eq -1) { break }
    $loadMsg += [char]$ch
    if ($loadMsg.EndsWith("`n")) { break }
}
Write-Host "  $($loadMsg.Trim())" -ForegroundColor DarkGray

# Read the banner lines from stdout (interpreter prints banner info)
# The interactive mode prints "> " prompt, but piped mode doesn't
# We're in interactive mode (stdin is a pipe that looks like a terminal?),
# actually no - let's check. With RedirectStandardInput, is_terminal() = false,
# so it will use piped mode. In piped mode it reads ALL stdin then evaluates.
# That means we can't do one-at-a-time with piped mode.
#
# Instead: write all expressions, close stdin, then read results line by line.

$pass = 0
$fail = 0

# Write all expressions to stdin at once
foreach ($test in $tests) {
    $proc.StandardInput.WriteLine($test.Expr)
}
$proc.StandardInput.Close()

# Now read results one at a time and compare live
for ($i = 0; $i -lt $tests.Count; $i++) {
    $line = $proc.StandardOutput.ReadLine()
    if ($null -eq $line) {
        Write-Host "FAIL [$($i+1)] $($tests[$i].Expr)" -ForegroundColor Red
        Write-Host "       expected: $($tests[$i].Expected)" -ForegroundColor Red
        Write-Host "       got:      (no output)" -ForegroundColor Red
        $fail++
        continue
    }

    $actual = $line.Trim()
    $expected = $tests[$i].Expected

    if ($actual -eq $expected) {
        Write-Host "PASS [$($i+1)] $($tests[$i].Expr)" -ForegroundColor Green
        $pass++
    } else {
        Write-Host "FAIL [$($i+1)] $($tests[$i].Expr)" -ForegroundColor Red
        Write-Host "       expected: $expected" -ForegroundColor Yellow
        Write-Host "       got:      $actual" -ForegroundColor Yellow
        $fail++
    }
}

# Check for extra output
$extra = @()
while ($null -ne ($line = $proc.StandardOutput.ReadLine())) {
    $t = $line.Trim()
    if ($t -ne "") { $extra += $t }
}
if ($extra.Count -gt 0) {
    Write-Host ""
    Write-Host "WARNING: $($extra.Count) extra output lines:" -ForegroundColor Yellow
    foreach ($e in $extra) {
        Write-Host "  $e" -ForegroundColor Yellow
    }
}

$proc.WaitForExit()

# Summary
Write-Host ""
Write-Host "Results: " -NoNewline
if ($fail -eq 0) {
    Write-Host "$pass/$($pass + $fail) passed" -ForegroundColor Green
    Write-Host "All tests passed!" -ForegroundColor Green
} else {
    Write-Host "$pass passed, $fail failed" -ForegroundColor Red
    Write-Host "out of $($pass + $fail) tests" -ForegroundColor Red
}

exit $fail
