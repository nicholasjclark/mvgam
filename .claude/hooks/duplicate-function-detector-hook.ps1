# Duplicate Function Name Detector Hook for R Packages (PowerShell)

param(
    [string]$Event,
    [string]$FilePath,
    [string]$Operation
)

# Configuration
$SMART_MODE = $true
$PERIODIC_MODE = $false
$FILES_THRESHOLD = 5
$MANUAL_TRIGGER_FILE = ".claude/run-duplicate-check"
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

# Find all R files in the package
function Find-RFilesInPackage {
    $rFiles = @()
    
    if (Test-Path ".\R") {
        $rFiles += Get-ChildItem -Path ".\R" -Filter "*.R" -Recurse | ForEach-Object { $_.FullName }
    }
    
    if (Test-Path ".\tests") {
        $rFiles += Get-ChildItem -Path ".\tests" -Filter "*.R" -Recurse | ForEach-Object { $_.FullName }
    }
    
    # Filter out .claude directory
    $rFiles = $rFiles | Where-Object { $_ -notlike "*\.claude\*" }
    
    return $rFiles | Sort-Object
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
        
        foreach ($funcName in $newFunctions) {
            if ($funcName -notin $existingFunctions) {
                Write-Host "New function detected: $funcName" -ForegroundColor Green
                return $true
            }
        }
    }
    
    return $false
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
        Write-Host "Manual trigger detected" -ForegroundColor Yellow
        Remove-Item -Path $MANUAL_TRIGGER_FILE -Force
        return $true
    }
    
    # Smart mode: only run when new functions are detected
    if ($SMART_MODE) {
        if (Test-HasNewFunctions -FilePath $FilePath -NewContent $NewContent -Operation $Operation) {
            Write-Host "New function(s) detected, running duplicate check" -ForegroundColor Green
            return $true
        }
        return $false
    }
    
    return $true
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
        return $true
    }
    
    Write-Host "Functions in new/modified content:" -ForegroundColor Cyan
    foreach ($func in $newFunctions) {
        Write-Host "  - $func" -ForegroundColor White
    }
    
    # Find all existing R files in the package
    $rFiles = Find-RFilesInPackage
    
    # Check for duplicates
    $duplicatesFound = @()
    
    foreach ($funcName in $newFunctions) {
        foreach ($existingFile in $rFiles) {
            if ((Test-Path $existingFile) -and ($existingFile -ne $TargetFile)) {
                $existingContent = Get-Content -Path $existingFile -Raw
                $existingFunctions = Get-FunctionNames -Content $existingContent
                
                if ($funcName -in $existingFunctions) {
                    $duplicatesFound += "Function '$funcName' already exists in: $existingFile"
                }
            }
        }
    }
    
    # Report findings
    if ($duplicatesFound.Count -gt 0) {
        Write-Host ""
        Write-Host "DUPLICATE FUNCTIONS DETECTED:" -ForegroundColor Red
        foreach ($duplicate in $duplicatesFound) {
            Write-Host "  $duplicate" -ForegroundColor Red
        }
        Write-Host ""
        Write-Host "Please resolve these duplicates before proceeding." -ForegroundColor Red
        return $false
    } else {
        Write-Host ""
        Write-Host "No duplicate functions detected." -ForegroundColor Green
        return $true
    }
}

# Main execution logic
try {
    Write-Host "Duplicate Function Detector Hook activated" -ForegroundColor Cyan
    Write-Host "Event: $Event, FilePath: $FilePath, Operation: $Operation"
    
    # Only process R files
    if (-not (Test-RFile -FilePath $FilePath)) {
        Write-Host "Skipping non-R file: $FilePath" -ForegroundColor Gray
        exit 0
    }
    
    # Read the new content
    $newContent = ""
    if (Test-Path $FilePath) {
        $newContent = Get-Content -Path $FilePath -Raw
    }
    
    # Determine if we should run the check
    if (-not (Test-ShouldRunCheck -FilePath $FilePath -NewContent $newContent -Operation $Operation)) {
        Write-Host "Skipping duplicate check (no new functions detected)" -ForegroundColor Gray
        exit 0
    }
    
    # Run the duplicate function check
    $result = Test-DuplicateFunctions -TargetFile $FilePath -NewContent $newContent -Operation $Operation
    
    if (-not $result) {
        Write-Host "Hook failed due to duplicate functions" -ForegroundColor Red
        exit 1
    }
    
    Write-Host "Hook completed successfully" -ForegroundColor Green
    exit 0
    
} catch {
    Write-Host "Error in duplicate function detector: $_" -ForegroundColor Red
    Write-Host "Stack trace: $($_.ScriptStackTrace)" -ForegroundColor Red
    exit 1
}