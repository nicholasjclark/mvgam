# R Code Review Hook for Claude Code (PowerShell)
# Save as: r-code-review-hook.ps1

param(
    [string]$Event,
    [string]$FilePath,
    [string]$Operation
)

$CODE_REVIEWER = "code-reviewer"

# Function to check if file is an R file
function Test-RFile {
    param([string]$FilePath)
    
    $extension = [System.IO.Path]::GetExtension($FilePath).ToLower()
    return $extension -in @('.r', '.rmd', '.rnw')
}

# Function to extract status from markdown review
function Get-ReviewStatus {
    param([string]$ReviewFile)
    
    if (-not (Test-Path $ReviewFile)) {
        return "UNKNOWN"
    }
    
    $content = Get-Content -Path $ReviewFile -Raw
    
    if ($content -match '\*\*Status\*\*:.*✅ APPROVED') {
        return "APPROVED"
    }
    elseif ($content -match '\*\*Status\*\*:.*❌ REJECTED') {
        return "REJECTED"
    }
    elseif ($content -match '\*\*Status\*\*:.*⚠️ NEEDS REVISION') {
        return "NEEDS_REVISION"
    }
    else {
        return "UNKNOWN"
    }
}

# Function to count high priority issues
function Get-HighPriorityIssueCount {
    param([string]$ReviewFile)
    
    if (-not (Test-Path $ReviewFile)) {
        return 0
    }
    
    $content = Get-Content -Path $ReviewFile
    $highPriorityCount = 0
    $inHighPrioritySection = $false
    
    foreach ($line in $content) {
        if ($line -match '^## 🚨 HIGH PRIORITY') {
            $inHighPrioritySection = $true
            continue
        }
        elseif ($line -match '^## ⚠️ MEDIUM PRIORITY|^## 💡 LOW PRIORITY|^## ✅ Positive') {
            $inHighPrioritySection = $false
            continue
        }
        
        if ($inHighPrioritySection -and $line -match '^- \*\*.*\*\*:') {
            $highPriorityCount++
        }
    }
    
    return $highPriorityCount
}

# Function to display review summary
function Show-ReviewSummary {
    param([string]$ReviewFile)
    
    Write-Host ""
    Write-Host "📋 CODE REVIEW SUMMARY" -ForegroundColor Cyan
    Write-Host "======================" -ForegroundColor Cyan
    
    # Extract and display status
    $status = Get-ReviewStatus -ReviewFile $ReviewFile
    switch ($status) {
        "APPROVED" {
            Write-Host "✅ Status: APPROVED" -ForegroundColor Green
        }
        "REJECTED" {
            Write-Host "❌ Status: REJECTED" -ForegroundColor Red
        }
        "NEEDS_REVISION" {
            Write-Host "⚠️  Status: NEEDS REVISION" -ForegroundColor Yellow
        }
        default {
            Write-Host "❓ Status: UNKNOWN" -ForegroundColor Gray
        }
    }
    
    # Show high priority issues count
    $highPriority = Get-HighPriorityIssueCount -ReviewFile $ReviewFile
    if ($highPriority -gt 0) {
        Write-Host "🚨 High Priority Issues: $highPriority (must be fixed)" -ForegroundColor Red
    }
    
    # Show key sections if they exist
    $content = Get-Content -Path $ReviewFile
    $inHighPrioritySection = $false
    $inActionSection = $false
    
    Write-Host ""
    
    # Display high priority issues
    foreach ($line in $content) {
        if ($line -match '^## 🚨 HIGH PRIORITY') {
            $inHighPrioritySection = $true
            Write-Host "🚨 HIGH PRIORITY ISSUES FOUND:" -ForegroundColor Red
            continue
        }
        elseif ($line -match '^## ⚠️ MEDIUM PRIORITY|^## 💡 LOW PRIORITY|^## ✅ Positive') {
            $inHighPrioritySection = $false
            continue
        }
        elseif ($line -match '^## 🎯 Action Required for Approval') {
            $inActionSection = $true
            Write-Host "🎯 ACTION REQUIRED:" -ForegroundColor Yellow
            continue
        }
        elseif ($line -match '^## 📊 Priority Summary|^## Architecture Analysis|^## Final Recommendation') {
            $inActionSection = $false
            continue
        }
        
        if ($inHighPrioritySection -and ($line -match '^### ❌' -or $line -match '^- \*\*.*\*\*:')) {
            Write-Host "  $line" -ForegroundColor White
        }
        elseif ($inActionSection -and $line -match '^\d+\.') {
            Write-Host "  $line" -ForegroundColor White
        }
    }
    
    Write-Host ""
}

# Function to prompt user for decision
function Read-UserChoice {
    param(
        [string]$Message,
        [string]$Default = "n"
    )
    
    if ($Default -eq "n") {
        $prompt = "$Message (y/N): "
    }
    else {
        $prompt = "$Message (Y/n): "
    }
    
    $response = Read-Host $prompt
    
    switch ($response.ToLower()) {
        "y" { return $true }
        "yes" { return $true }
        "n" { return $false }
        "no" { return $false }
        "" { return $Default -eq "y" }
        default { return $false }
    }
}

# Function to review R code
function Invoke-RCodeReview {
    param(
        [string]$FilePath,
        [string]$Content,
        [string]$Operation
    )
    
    # Create temporary files
    $tempCode = [System.IO.Path]::GetTempFileName()
    $tempReview = [System.IO.Path]::GetTempFileName()
    
    try {
        # Write content to temporary file
        $Content | Out-File -FilePath $tempCode -Encoding utf8
        
        # Create review prompt
        $reviewPrompt = @"
Please review this R code change:

File: $FilePath
Operation: $Operation

Please analyze the code in the attached file for:
1. Code quality and R best practices
2. Potential bugs or issues  
3. Performance considerations
4. Documentation completeness
5. Package development standards

Use your standard markdown template format for the review.
"@
        
        Write-Host "🔍 Sending code to reviewer..." -ForegroundColor Cyan
        
        # Call code-reviewer sub-agent
        $processInfo = New-Object System.Diagnostics.ProcessStartInfo
        $processInfo.FileName = "claude-code"
        $processInfo.Arguments = "sub-agent $CODE_REVIEWER --input `"$tempCode`" --prompt `"$reviewPrompt`""
        $processInfo.RedirectStandardOutput = $true
        $processInfo.RedirectStandardError = $true
        $processInfo.UseShellExecute = $false
        $processInfo.CreateNoWindow = $true
        
        $process = New-Object System.Diagnostics.Process
        $process.StartInfo = $processInfo
        
        if ($process.Start()) {
            $output = $process.StandardOutput.ReadToEnd()
            $error = $process.StandardError.ReadToEnd()
            $process.WaitForExit()
            
            if ($process.ExitCode -eq 0 -and $output.Trim()) {
                $output | Out-File -FilePath $tempReview -Encoding utf8
                
                Show-ReviewSummary -ReviewFile $tempReview
                
                $status = Get-ReviewStatus -ReviewFile $tempReview
                $highPriority = Get-HighPriorityIssueCount -ReviewFile $tempReview
                
                # Decision logic based on review
                switch ($status) {
                    "APPROVED" {
                        Write-Host "✅ Code review passed!" -ForegroundColor Green
                        return $true
                    }
                    "REJECTED" {
                        Write-Host "❌ Code review rejected!" -ForegroundColor Red
                        return Read-UserChoice -Message "Proceed anyway?" -Default "n"
                    }
                    "NEEDS_REVISION" {
                        if ($highPriority -gt 0) {
                            Write-Host "⚠️  Code needs revision with $highPriority high priority issues!" -ForegroundColor Yellow
                            return Read-UserChoice -Message "Proceed anyway?" -Default "n"
                        }
                        else {
                            Write-Host "⚠️  Code needs minor revisions but no critical issues found." -ForegroundColor Yellow
                            return Read-UserChoice -Message "Proceed?" -Default "y"
                        }
                    }
                    default {
                        Write-Host "❓ Could not determine review status." -ForegroundColor Gray
                        return Read-UserChoice -Message "Proceed anyway?" -Default "n"
                    }
                }
            }
            else {
                Write-Host "❌ No review response received or process failed" -ForegroundColor Red
                Write-Host "Error: $error" -ForegroundColor Red
                return $false
            }
        }
        else {
            Write-Host "❌ Failed to start code reviewer process" -ForegroundColor Red
            return $false
        }
    }
    finally {
        # Clean up temporary files
        if (Test-Path $tempCode) { Remove-Item -Path $tempCode -Force }
        if (Test-Path $tempReview) { Remove-Item -Path $tempReview -Force }
    }
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
    
    Write-Host "🔍 Reviewing R code changes for: $FilePath" -ForegroundColor Cyan
    
    # Read new content from stdin
    $newContent = @()
    while ($null -ne ($line = [Console]::ReadLine())) {
        $newContent += $line
    }
    $newContentString = $newContent -join "`n"
    
    # Review the code
    if (Invoke-RCodeReview -FilePath $FilePath -Content $newContentString -Operation $Operation) {
        Write-Host "✅ Proceeding with write operation" -ForegroundColor Green
        exit 0
    }
    else {
        Write-Host "❌ Aborting write operation" -ForegroundColor Red
        exit 1
    }
}
catch {
    Write-Host "❌ Error in code review hook: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}
