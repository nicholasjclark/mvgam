# Package-Wide Function Dependency Mapper Hook
param([string]$Event, [string]$CommitHash, [string]$CommitMessage)

# Configuration
$CACHE_FILE = ".claude/package-dependency-map.json"
$METADATA_FILE = ".claude/dependency-metadata.json"
$VISUALIZATION_FILE = ".claude/dependency-graph.md"

function Get-PackageName {
    if (Test-Path "DESCRIPTION") {
        $content = Get-Content "DESCRIPTION"
        $packageLine = $content | Where-Object { $_ -match "^Package:" }
        if ($packageLine) {
            return ($packageLine -split ":\s*")[1].Trim()
        }
    }
    return "unknown"
}

function Get-PackageVersion {
    if (Test-Path "DESCRIPTION") {
        $content = Get-Content "DESCRIPTION"
        $versionLine = $content | Where-Object { $_ -match "^Version:" }
        if ($versionLine) {
            return ($versionLine -split ":\s*")[1].Trim()
        }
    }
    return "0.0.0"
}

function Get-AllRFiles {
    $rFiles = @()
    if (Test-Path "R") {
        $rFiles += Get-ChildItem -Path "R" -Filter "*.R" -Recurse | ForEach-Object { $_.FullName }
    }
    if (Test-Path "src") {
        $rFiles += Get-ChildItem -Path "src" -Filter "*.R" -Recurse | ForEach-Object { $_.FullName }
    }
    return $rFiles
}

function Get-FunctionDefinitions {
    param([string[]]$Files)
    
    $functions = @{}
    $definitions = @{}
    
    foreach ($file in $Files) {
        Write-Host "Analyzing $file..." -ForegroundColor Blue
        
        if (!(Test-Path $file)) {
            continue
        }
        
        $content = Get-Content $file -Raw -ErrorAction SilentlyContinue
        if (!$content) {
            continue
        }
        
        $fileFunctions = @()
        
        # Simple function definition patterns
        $patterns = @(
            '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*function\s*\(',
            '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*=\s*function\s*\('
        )
        
        foreach ($pattern in $patterns) {
            $matches = [regex]::Matches($content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
            foreach ($match in $matches) {
                $functionName = $match.Groups[1].Value
                
                if ($functionName -and $functionName.Trim() -ne "") {
                    $cleanName = $functionName.Trim()
                    $fileFunctions += $cleanName
                    $functions[$cleanName] = $file
                }
            }
        }
        
        if ($fileFunctions.Count -gt 0) {
            $definitions[$file] = $fileFunctions
            Write-Host "Found $($fileFunctions.Count) functions: $($fileFunctions -join ', ')" -ForegroundColor Green
        }
    }
    
    return @{
        functions = $functions
        definitions = $definitions
    }
}

function New-PackageDependencyMap {
    Write-Host "Creating package-wide function dependency map..." -ForegroundColor Cyan
    
    $startTime = Get-Date
    
    # Initialize the dependency map structure
    $dependencyMap = @{
        metadata = @{
            generated = $startTime.ToString("yyyy-MM-dd HH:mm:ss")
            commit_hash = $CommitHash
            commit_message = $CommitMessage
            package_name = Get-PackageName
            version = Get-PackageVersion
            total_files = 0
            total_functions = 0
        }
        functions = @{}
        definitions = @{}
    }
    
    # Discover all R files
    $rFiles = Get-AllRFiles
    $dependencyMap.metadata.total_files = $rFiles.Count
    
    Write-Host "Analyzing $($rFiles.Count) R files..." -ForegroundColor Green
    
    # Find function definitions
    Write-Host "Step 1: Discovering function definitions..." -ForegroundColor Yellow
    $funcResults = Get-FunctionDefinitions -Files $rFiles
    $dependencyMap.functions = $funcResults.functions
    $dependencyMap.definitions = $funcResults.definitions
    $dependencyMap.metadata.total_functions = $funcResults.functions.Count
    
    # Save results
    Write-Host "Step 2: Saving dependency map..." -ForegroundColor Yellow
    
    # Save main dependency map
    $dependencyMap | ConvertTo-Json -Depth 10 | Out-File -FilePath $CACHE_FILE -Encoding UTF8
    
    # Save metadata separately
    $dependencyMap.metadata | ConvertTo-Json -Depth 5 | Out-File -FilePath $METADATA_FILE -Encoding UTF8
    
    # Create markdown visualization
    $markdown = @"
# Package Dependency Map

**Generated:** $($dependencyMap.metadata.generated)  
**Package:** $($dependencyMap.metadata.package_name) v$($dependencyMap.metadata.version)  
**Commit:** $($dependencyMap.metadata.commit_hash)  

## Summary

- **Total Files:** $($dependencyMap.metadata.total_files)
- **Total Functions:** $($dependencyMap.metadata.total_functions)

## Function Definitions by File

"@

    foreach ($file in $dependencyMap.definitions.Keys) {
        $funcs = $dependencyMap.definitions[$file] -join ", "
        $markdown += "`n### $file`n`n$funcs`n"
    }
    
    $markdown | Out-File -FilePath $VISUALIZATION_FILE -Encoding UTF8
    
    $endTime = Get-Date
    $duration = ($endTime - $startTime).TotalSeconds
    
    Write-Host ""
    Write-Host "Package dependency mapping complete!" -ForegroundColor Green
    Write-Host "Duration: $([math]::Round($duration, 2)) seconds" -ForegroundColor Cyan
    Write-Host "Check .claude/ directory for detailed maps and reports" -ForegroundColor Cyan
    Write-Host ""
}

# Main execution
try {
    if ($Event -eq "post_git_commit" -or $Event -eq "test") {
        New-PackageDependencyMap
    } else {
        Write-Host "Hook triggered for event: $Event (no action taken)" -ForegroundColor Gray
    }
} catch {
    Write-Host "Error in dependency mapper: $_" -ForegroundColor Red
    Write-Host "Stack trace: $($_.ScriptStackTrace)" -ForegroundColor Red
}