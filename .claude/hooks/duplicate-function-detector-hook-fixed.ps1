# Duplicate Function Detector - Fixed for Claude Code PostToolUse
# This version works without parameters and exits with code 2 to show output

Write-Host "Duplicate Function Detector activated" -ForegroundColor Cyan

# Find the most recently modified R file
$rFiles = Get-ChildItem -Path "R" -Filter "*.R" -ErrorAction SilentlyContinue | Sort-Object LastWriteTime -Descending

if ($rFiles.Count -eq 0) {
    Write-Host "No R files found in R/ directory" -ForegroundColor Yellow
    exit 2
}

$mostRecentFile = $rFiles[0].FullName
Write-Host "Checking for duplicate functions in: $mostRecentFile" -ForegroundColor Green

# Extract function names from the recent file
function Get-FunctionNames {
    param([string]$Content)
    
    $functions = @()
    $lines = $Content -split "`n"
    
    foreach ($line in $lines) {
        if ($line -match '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*function\s*\(') {
            $functions += $matches[1]
        }
        elseif ($line -match '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*=\s*function\s*\(') {
            $functions += $matches[1]
        }
    }
    
    return $functions | Sort-Object -Unique
}

# Read the recent file
$content = Get-Content -Path $mostRecentFile -Raw -ErrorAction SilentlyContinue
if (-not $content) {
    Write-Host "Could not read file content" -ForegroundColor Yellow
    exit 2
}

$newFunctions = Get-FunctionNames -Content $content

if ($newFunctions.Count -eq 0) {
    Write-Host "No functions found in file" -ForegroundColor Gray
    exit 2
}

Write-Host "Functions in file:" -ForegroundColor Cyan
foreach ($func in $newFunctions) {
    Write-Host "  - $func" -ForegroundColor White
}

# Check for duplicates in other R files
$allRFiles = Get-ChildItem -Path "R" -Filter "*.R" | Where-Object { $_.FullName -ne $mostRecentFile }
$duplicatesFound = @()

foreach ($funcName in $newFunctions) {
    foreach ($otherFile in $allRFiles) {
        $otherContent = Get-Content -Path $otherFile.FullName -Raw -ErrorAction SilentlyContinue
        if ($otherContent) {
            $otherFunctions = Get-FunctionNames -Content $otherContent
            if ($funcName -in $otherFunctions) {
                $duplicatesFound += "Function '$funcName' also exists in: $($otherFile.Name)"
            }
        }
    }
}

if ($duplicatesFound.Count -gt 0) {
    Write-Host ""
    Write-Host "DUPLICATE FUNCTIONS DETECTED:" -ForegroundColor Red
    foreach ($duplicate in $duplicatesFound) {
        Write-Host "  $duplicate" -ForegroundColor Red
    }
    Write-Host ""
    Write-Host "Please resolve these duplicates." -ForegroundColor Red
} else {
    Write-Host ""
    Write-Host "No duplicate functions detected." -ForegroundColor Green
}

Write-Host "Duplicate Function check completed" -ForegroundColor Cyan
exit 2