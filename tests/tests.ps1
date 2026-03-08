$bf = '.\Brainfuck64.exe'
$testFiles = Get-ChildItem -Path tests -Filter '*.bf'

$failed = 0
foreach ($test in $testFiles) {
    $name = $test.BaseName
    $expectedFile = "tests/$name.expected"
    if (Test-Path $expectedFile) {
        Write-Host "Running test $name..." -NoNewline
        $output = & $bf $test.FullName | Out-String
        $expected = Get-Content $expectedFile -Raw
        
        # Normalize newlines for comparison
        $output = $output.Replace("`r`n", "`n")
        $expected = $expected.Replace("`r`n", "`n")

        if ($output -eq $expected) {
            Write-Host ' PASSED' -ForegroundColor Green
        }
        else {
            Write-Host ' FAILED' -ForegroundColor Red
            Write-Host "Expected: [$expected]"
            Write-Host "Got:      [$output]"
            $failed++
        }
    }
}

if ($failed -gt 0) {
    exit 1
}
exit 0
