# R Code Review Hook (PowerShell)

param(
    [string]$Event,
    [string]$FilePath,
    [string]$Operation
)

# Configuration
$ENABLE_STYLE_CHECKS = $true
$ENABLE_COMPLEXITY_CHECKS = $true
$ENABLE_DOCUMENTATION_CHECKS = $true
$MAX_FUNCTION_LENGTH = 50
$MAX_CYCLOMATIC_COMPLEXITY = 10

# Function to check if file is an R file
function Test-RFile {
    param([string]$FilePath)
    $extension = [System.IO.Path]::GetExtension($FilePath).ToLower()
    return $extension -in @('.r', '.rmd', '.rnw')
}

# Check for common R style issues
function Test-RStyleIssues {
    param([string]$Content)
    
    $issues = @()
    $lines = $Content -split "`n"
    
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
        
        # Check for tabs (should use spaces)
        if ($line -match '\t') {
            $issues += "Line $lineNum contains tabs (use spaces instead)"
        }
        
        # Check for assignment operator style
        if ($line -match '\s*=\s*' -and $line -notmatch '==|!=|<=|>=') {
            if ($line -match 'function\s*\(.*\)\s*=') {
                # Skip function assignments
            } elseif ($line -match '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*=\s*') {
                $issues += "Line $lineNum uses '=' for assignment (prefer '<-')"
            }
        }
    }
    
    return $issues
}

# Check for overly complex functions
function Test-FunctionComplexity {
    param([string]$Content)
    
    $issues = @()
    $lines = $Content -split "`n"
    $inFunction = $false
    $functionStart = 0
    $functionName = ""
    $braceCount = 0
    
    for ($i = 0; $i -lt $lines.Length; $i++) {
        $line = $lines[$i]
        $lineNum = $i + 1
        
        # Detect function start
        if ($line -match '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*function\s*\(') {
            $functionName = $matches[1]
            $inFunction = $true
            $functionStart = $lineNum
            $braceCount = 0
        }
        
        if ($inFunction) {
            # Count braces to detect function end
            $openBraces = ($line -split '\{').Length - 1
            $closeBraces = ($line -split '\}').Length - 1
            $braceCount += $openBraces - $closeBraces
            
            # Function ended
            if ($braceCount -eq 0 -and $line -match '\}') {
                $functionLength = $lineNum - $functionStart + 1
                if ($functionLength -gt $MAX_FUNCTION_LENGTH) {
                    $issues += "Function '$functionName' is too long ($functionLength lines, max $MAX_FUNCTION_LENGTH)"
                }
                $inFunction = $false
            }
        }
    }
    
    return $issues
}

# Check for missing documentation
function Test-Documentation {
    param([string]$Content)
    
    $issues = @()
    $lines = $Content -split "`n"
    
    for ($i = 0; $i -lt $lines.Length; $i++) {
        $line = $lines[$i]
        $lineNum = $i + 1
        
        # Check for exported functions without documentation
        if ($line -match '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*function\s*\(') {
            $functionName = $matches[1]
            
            # Look for roxygen2 comments above the function
            $hasDocumentation = $false
            for ($j = $i - 1; $j -ge 0; $j--) {
                $prevLine = $lines[$j].Trim()
                if ($prevLine -eq "") {
                    continue
                }
                if ($prevLine -match "^#'") {
                    $hasDocumentation = $true
                    break
                }
                if ($prevLine -notmatch "^#") {
                    break
                }
            }
            
            if (-not $hasDocumentation) {
                $issues += "Function '$functionName' at line $lineNum lacks roxygen2 documentation"
            }
        }
    }
    
    return $issues
}

# Main review function
function Test-RCodeQuality {
    param(
        [string]$FilePath,
        [string]$Content
    )
    
    $allIssues = @()
    
    if ($ENABLE_STYLE_CHECKS) {
        $styleIssues = Test-RStyleIssues -Content $Content
        if ($styleIssues.Count -gt 0) {
            $allIssues += "STYLE ISSUES:"
            $allIssues += $styleIssues
        }
    }
    
    if ($ENABLE_COMPLEXITY_CHECKS) {
        $complexityIssues = Test-FunctionComplexity -Content $Content
        if ($complexityIssues.Count -gt 0) {
            $allIssues += ""
            $allIssues += "COMPLEXITY ISSUES:"
            $allIssues += $complexityIssues
        }
    }
    
    if ($ENABLE_DOCUMENTATION_CHECKS) {
        $docIssues = Test-Documentation -Content $Content
        if ($docIssues.Count -gt 0) {
            $allIssues += ""
            $allIssues += "DOCUMENTATION ISSUES:"
            $allIssues += $docIssues
        }
    }
    
    return $allIssues
}

# Main execution logic
try {
    Write-Host "R Code Review Hook activated" -ForegroundColor Cyan
    Write-Host "Event: $Event, FilePath: $FilePath, Operation: $Operation"
    
    # Only process R files
    if (-not (Test-RFile -FilePath $FilePath)) {
        Write-Host "Skipping non-R file: $FilePath" -ForegroundColor Gray
        exit 0
    }
    
    # Read the file content
    if (-not (Test-Path $FilePath)) {
        Write-Host "File not found: $FilePath" -ForegroundColor Yellow
        exit 0
    }
    
    $content = Get-Content -Path $FilePath -Raw
    if (-not $content) {
        Write-Host "Empty file: $FilePath" -ForegroundColor Gray
        exit 0
    }
    
    # Run code quality checks
    $issues = Test-RCodeQuality -FilePath $FilePath -Content $content
    
    if ($issues.Count -gt 0) {
        Write-Host ""
        Write-Host "CODE REVIEW FINDINGS for ${FilePath}:" -ForegroundColor Yellow
        Write-Host "="*60 -ForegroundColor Yellow
        foreach ($issue in $issues) {
            if ($issue -eq "") {
                Write-Host ""
            } elseif ($issue -match "^[A-Z ]+ISSUES:$") {
                Write-Host $issue -ForegroundColor Cyan
            } else {
                Write-Host "  $issue" -ForegroundColor White
            }
        }
        Write-Host "="*60 -ForegroundColor Yellow
        Write-Host ""
        Write-Host "Consider addressing these issues to improve code quality." -ForegroundColor Yellow
    } else {
        Write-Host ""
        Write-Host "No code quality issues found in ${FilePath}" -ForegroundColor Green
    }
    
    Write-Host "Hook completed successfully" -ForegroundColor Green
    exit 0
    
} catch {
    Write-Host "Error in R code review: $_" -ForegroundColor Red
    Write-Host "Stack trace: $($_.ScriptStackTrace)" -ForegroundColor Red
    exit 1
}