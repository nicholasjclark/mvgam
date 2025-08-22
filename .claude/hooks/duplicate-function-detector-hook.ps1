# Duplicate Function Name Detector Hook for R Packages (PowerShell)
# Save as: duplicate-function-detector-hook.ps1

param(
    [string]$Event,
    [string]$FilePath,
    [string]$Operation
)

# Configuration - Set these based on your preferences
$SMART_MODE = $true              # Only run when new functions are detected
$PERIODIC_MODE = $false          # Run periodically based on file count
$FILES_THRESHOLD = 5             # Run every N file changes (if PERIODIC_MODE=$true)
$MANUAL_TRIGGER_FILE = ".claude/run-duplicate-check"  # Create this file to force a run

# Counter file for periodic mode
$COUNTER_FILE = ".claude/duplicate-check-counter"

# Function to check if file is an R file
function Test-RFile {
    param([string]$FilePath)
    
    $extension = [System.IO.Path]::GetExtension($FilePath).ToLower()
    return $extension -in @('.r', '.rmd', '.rnw')
}

# Extract function names from R content
function Get-FunctionNames {
    param([string]$Content)
    
    $functions = @()
    
    # Split content into lines
    $lines = $Content -split "`n"
    
    foreach ($line in $lines) {
        # Standard function definition: name <- function(...)
        if ($line -match '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*function\s*\(') {
            $functions += $matches[1]
        }
        # Alternative assignment: name = function(...)
        elseif ($line -match '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*=\s*function\s*\(') {
            $functions += $matches[1]
        }
        # Function assignment with quotes: "name" <- function(...)
        elseif ($line -match '^\s*["\x27]([a-zA-Z_][a-zA-Z0-9_.]*)["\x27]\s*<-\s*function\s*\(') {
            $functions += $matches[1]
        }
    }
    
    return $functions | Sort-Object -Unique
}

# Find all R files in the package
function Find-RFilesInPackage {
    $rFiles = @()
    
    # Look for R files in standard package locations
    if (Test-Path ".\R") {
        $rFiles += Get-ChildItem -Path ".\R" -Filter "*.R" -Recurse | ForEach-Object { $_.FullName }
        $rFiles += Get-ChildItem -Path ".\R" -Filter "*.r" -Recurse | ForEach-Object { $_.FullName }
    }
    
    if (Test-Path ".\tests") {
        $rFiles += Get-ChildItem -Path ".\tests" -Filter "*.R" -Recurse | ForEach-Object { $_.FullName }
        $rFiles += Get-ChildItem -Path ".\tests" -Filter "*.r" -Recurse | ForEach-Object { $_.FullName }
    }
    
    if (Test-Path ".\inst") {
        $rFiles += Get-ChildItem -Path ".\inst" -Filter "*.R" -Recurse | ForEach-Object { $_.FullName }
        $rFiles += Get-ChildItem -Path ".\inst" -Filter "*.r" -Recurse | ForEach-Object { $_.FullName }
    }
    
    # Filter out .claude directory and convert to relative paths
    $rFiles = $rFiles | Where-Object { $_ -notlike "*\.claude\*" } | ForEach-Object {
        $_.Replace((Get-Location).Path + "\", ".\").Replace("\", "/")
    }
    
    return $rFiles | Sort-Object
}

# Extract function signature from file
function Get-FunctionSignature {
    param(
        [string]$FunctionName,
        [string]$FilePath
    )
    
    if (-not (Test-Path $FilePath)) {
        return ""
    }
    
    $content = Get-Content -Path $FilePath -Raw
    $lines = $content -split "`n"
    
    for ($i = 0; $i -lt $lines.Length; $i++) {
        if ($lines[$i] -match "^\s*$FunctionName\s*<-\s*function\s*\(") {
            $signature = $lines[$i].Trim()
            
            # If the line ends with just "function(" we need to get the parameters
            if ($lines[$i] -match 'function\s*\(\s*$') {
                $j = $i + 1
                while ($j -lt $lines.Length -and $lines[$j] -notmatch '\)\s*\{?\s*$') {
                    $signature += " " + $lines[$j].Trim()
                    $j++
                }
                if ($j -lt $lines.Length) {
                    $signature += " " + $lines[$j].Trim()
                }
            }
            
            # Clean up the signature
            $signature = $signature -replace '\s*\{\s*$', ''
            return $signature
        }
    }
    
    return ""
}

# Check if new functions are being added
function Test-HasNewFunctions {
    param(
        [string]$FilePath,
        [string]$NewContent,
        [string]$Operation
    )
    
    $newFunctions = Get-FunctionNames -Content $NewContent
    
    if ($newFunctions.Count -eq 0) {
        return $false
    }
    
    # For create operations, any function is "new"
    if ($Operation -eq "create" -or -not (Test-Path $FilePath)) {
        return $true
    }
    
    # For modify operations, compare with existing content
    if (Test-Path $FilePath) {
        $existingContent = Get-Content -Path $FilePath -Raw
        $existingFunctions = Get-FunctionNames -Content $existingContent
        
        # Check if any function in new content is not in existing content
        foreach ($funcName in $newFunctions) {
            if ($funcName -notin $existingFunctions) {
                Write-Host "🆕 New function detected: $funcName" -ForegroundColor Green
                return $true
            }
        }
    }
    
    return $false
}

# Periodic mode helper functions
function Get-FileChangeCount {
    if (Test-Path $COUNTER_FILE) {
        return [int](Get-Content -Path $COUNTER_FILE -Raw).Trim()
    }
    return 0
}

function Add-FileChangeCount {
    $count = Get-FileChangeCount
    ($count + 1) | Out-File -FilePath $COUNTER_FILE -Encoding utf8
}

function Reset-FileChangeCount {
    0 | Out-File -FilePath $COUNTER_FILE -Encoding utf8
}

# Determine whether to run the duplicate check
function Test-ShouldRunCheck {
    param(
        [string]$FilePath,
        [string]$NewContent,
        [string]$Operation
    )
    
    # Always run if manual trigger file exists
    if (Test-Path $MANUAL_TRIGGER_FILE) {
        Write-Host "🔄 Manual trigger detected" -ForegroundColor Yellow
        Remove-Item -Path $MANUAL_TRIGGER_FILE -Force
        return $true
    }
    
    # Smart mode: only run when new functions are detected
    if ($SMART_MODE) {
        if (Test-HasNewFunctions -FilePath $FilePath -NewContent $NewContent -Operation $Operation) {
            Write-Host "📝 New function(s) detected, running duplicate check" -ForegroundColor Green
            return $true
        }
        return $false
    }
    
    # Periodic mode: run every N file changes
    if ($PERIODIC_MODE) {
        $count = Get-FileChangeCount
        if ($count -ge $FILES_THRESHOLD) {
            Write-Host "📊 Periodic check triggered ($count file changes)" -ForegroundColor Yellow
            Reset-FileChangeCount
            return $true
        }
        else {
            Add-FileChangeCount
            Write-Host "📈 File change count: $count/$FILES_THRESHOLD" -ForegroundColor Gray
            return $false
        }
    }
    
    # Default: always run (original behavior)
    return $true
}

# Show function signatures for comparison
function Show-FunctionSignatures {
    param(
        [string[]]$NewFunctions,
        [string[]]$RFiles
    )
    
    Write-Host ""
    Write-Host "🔍 FUNCTION SIGNATURES COMPARISON:" -ForegroundColor Cyan
    
    foreach ($funcName in $NewFunctions) {
        Write-Host ""
        Write-Host "📋 Function: $funcName" -ForegroundColor Yellow
        
        # Find existing implementations
        foreach ($existingFile in $RFiles) {
            if (Test-Path $existingFile) {
                $signature = Get-FunctionSignature -FunctionName $funcName -FilePath $existingFile
                if ($signature) {
                    Write-Host "  📁 $existingFile:" -ForegroundColor Gray
                    Write-Host "     $signature" -ForegroundColor White
                }
            }
        }
    }
}

# Main duplicate checking function
function Test-DuplicateFunctions {
    param(
        [string]$TargetFile,
        [string]$NewContent,
        [string]$Operation
    )
    
    # Extract function names from new content
    $newFunctions = Get-FunctionNames -Content $NewContent
    
    if ($newFunctions.Count -eq 0) {
        return $true  # No functions to check
    }
    
    Write-Host "📋 Functions in new/modified content:" -ForegroundColor Cyan
    foreach ($func in $newFunctions) {
        Write-Host "  - $func" -ForegroundColor White
    }
    
    # Find all existing R files in the package
    $rFiles = Find-RFilesInPackage
    
    # Check for duplicates
    $duplicatesFound = @()
    $potentialConflicts = @()
    
    foreach ($funcName in $newFunctions) {
        # Check each existing R file
        foreach ($existingFile in $rFiles) {
            if ((Test-Path $existingFile) -and ($existingFile -ne $TargetFile)) {
                $existingContent = Get-Content -Path $existingFile -Raw
                $existingFunctions = Get-FunctionNames -Content $existingContent
                
                if ($funcName -in $existingFunctions) {
                    $duplicatesFound += "  🔴 '$funcName' already exists in: $existingFile"
                }
            }
        }
        
        # For modify operations, also check if function already exists in the same file
        if ($Operation -eq "modify" -and (Test-Path $TargetFile)) {
            $currentContent = Get-Content -Path $TargetFile -Raw
            $currentFunctions = Get-FunctionNames -Content $currentContent
            if ($funcName -in $currentFunctions) {
                $potentialConflicts += "  🟡 '$funcName' being redefined in same file: $TargetFile"
            }
        }
    }
    
    # Report findings
    $hasIssues = $false
    
    if ($duplicatesFound.Count -gt 0) {
        Write-Host ""
        Write-Host "❌ DUPLICATE FUNCTIONS DETECTED:" -ForegroundColor Red
        foreach ($duplicate in $duplicatesFound) {
            Write-Host $duplicate -ForegroundColor Red
        }
        $hasIssues = $true
    }
    
    if ($potentialConflicts.Count -gt 0) {
        Write-Host ""
        Write-Host "⚠️  POTENTIAL REDEFINITIONS:" -ForegroundColor Yellow
        foreach ($conflict in $potentialConflicts) {
            Write-Host $conflict -ForegroundColor Yellow
        }
    }
    
    if ($hasIssues) {
        Write-Host ""
        Write-Host "🔧 RESOLUTION OPTIONS:" -ForegroundColor Cyan
        Write-Host "  1. Rename the function(s) to be unique" -ForegroundColor White
        Write-Host "  2. Remove duplicate implementation" -ForegroundColor White
        Write-Host "  3. Merge implementations if appropriate" -ForegroundColor White
        Write-Host "  4. Move to existing file if it's the same function" -ForegroundColor White
        
        # Show function signatures for comparison
        Show-FunctionSignatures -NewFunctions $newFunctions -RFiles $rFiles
        
        Write-Host ""
        $response = Read-Host "Do you want to proceed anyway? (This will create duplicate functions) (y/N)"
        
        if ($response -match '^[Yy]') {
            Write-Host "⚠️  Proceeding with duplicate functions - consider resolving later" -ForegroundColor Yellow
            return $true
        }
        else {
            Write-Host "❌ Aborting to avoid duplicate functions" -ForegroundColor Red
            return $false
        }
    }
    
    return $true
}

# Main execution
try {
    # Only handle before_write events for R files
    if ($Event -ne "before_write" -or -not (Test-RFile -FilePath $FilePath)) {
        exit 0
    }
    
    # Skip for delete operations
    if ($Operation -eq "delete") {
        exit 0
    }
    
    # Read new content from stdin
    $newContent = @()
    while ($null -ne ($line = [Console]::ReadLine())) {
        $newContent += $line
    }
    $newContentString = $newContent -join "`n"
    
    # Determine if we should run the check
    if (-not (Test-ShouldRunCheck -FilePath $FilePath -NewContent $newContentString -Operation $Operation)) {
        Write-Host "⏭️  Skipping duplicate function check (no new functions detected)" -ForegroundColor Gray
        exit 0
    }
    
    Write-Host "🔍 Checking for duplicate function names in R package..." -ForegroundColor Cyan
    
    # Check for duplicates
    if (Test-DuplicateFunctions -TargetFile $FilePath -NewContent $newContentString -Operation $Operation) {
        Write-Host "✅ No duplicate functions detected" -ForegroundColor Green
        exit 0
    }
    else {
        Write-Host "❌ Duplicate functions found - aborting write operation" -ForegroundColor Red
        exit 1
    }
}
catch {
    Write-Host "❌ Error in duplicate function detector: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}
