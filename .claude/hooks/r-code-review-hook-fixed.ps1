# R Code Review Hook - Fixed for Claude Code PostToolUse
# This version works without parameters and exits with code 2 to show output

Write-Host "R Code Review Hook activated" -ForegroundColor Cyan

# Find the most recently modified R file (likely the one that was just edited)
$rFiles = Get-ChildItem -Path "R" -Filter "*.R" -ErrorAction SilentlyContinue | Sort-Object LastWriteTime -Descending

if ($rFiles.Count -eq 0) {
    Write-Host "No R files found in R/ directory" -ForegroundColor Yellow
    exit 2
}

$mostRecentFile = $rFiles[0].FullName
Write-Host "Reviewing most recently modified file: $mostRecentFile" -ForegroundColor Green

# Read file content
if (-not (Test-Path $mostRecentFile)) {
    Write-Host "File not found: $mostRecentFile" -ForegroundColor Red
    exit 2
}

$content = Get-Content -Path $mostRecentFile -Raw
if (-not $content) {
    Write-Host "File is empty: $mostRecentFile" -ForegroundColor Yellow
    exit 2
}

# Simple style checks
$issues = @()
$lines = $content -split "`n"

for ($i = 0; $i -lt $lines.Length; $i++) {
    $line = $lines[$i]
    $lineNum = $i + 1
    
    # Check line length (80 characters)
    if ($line.Length -gt 80) {
        $issues += "Line $lineNum exceeds 80 characters ($($line.Length) chars)"
    }
    
    # Check for trailing whitespace
    if ($line -match '\s+$') {
        $issues += "Line $lineNum has trailing whitespace"
    }
    
    # Check for assignment operator style (basic check)
    if ($line -match '\s+=' -and $line -notmatch '==|!=|<=|>=' -and $line -match '^\s*[a-zA-Z_][a-zA-Z0-9_.]*\s*=\s*[^=]') {
        $issues += "Line $lineNum uses '=' for assignment (prefer '<-')"
    }
}

if ($issues.Count -gt 0) {
    Write-Host ""
    Write-Host "CODE REVIEW FINDINGS:" -ForegroundColor Yellow
    Write-Host "="*50 -ForegroundColor Yellow
    foreach ($issue in $issues) {
        Write-Host "  $issue" -ForegroundColor White
    }
    Write-Host "="*50 -ForegroundColor Yellow
    Write-Host "Consider addressing these style issues." -ForegroundColor Yellow
} else {
    Write-Host "No code style issues found!" -ForegroundColor Green
}

Write-Host "R Code Review completed" -ForegroundColor Cyan
exit 2