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

function Get-ExportedFunctions {
    $exportedFunctions = @()
    if (Test-Path "NAMESPACE") {
        $namespaceContent = Get-Content "NAMESPACE"
        foreach ($line in $namespaceContent) {
            # Match export() lines
            if ($line -match '^export\((.+)\)$') {
                $funcName = $matches[1].Trim('"')
                $exportedFunctions += $funcName
            }
        }
    }
    return $exportedFunctions
}

function Get-AllRFiles {
    $rFiles = @()
    $currentDir = (Get-Location).Path
    if (Test-Path "R") {
        $rFiles += Get-ChildItem -Path "R" -Filter "*.R" -Recurse | ForEach-Object { 
            # Create relative path by removing the current directory from full path
            $fullPath = $_.FullName
            if ($fullPath.StartsWith($currentDir)) {
                $relativePath = $fullPath.Substring($currentDir.Length).TrimStart('\', '/')
            } else {
                $relativePath = $fullPath
            }
            $relativePath -replace '\\', '/'
        }
    }
    if (Test-Path "src") {
        $rFiles += Get-ChildItem -Path "src" -Filter "*.R" -Recurse | ForEach-Object { 
            # Create relative path by removing the current directory from full path
            $fullPath = $_.FullName
            if ($fullPath.StartsWith($currentDir)) {
                $relativePath = $fullPath.Substring($currentDir.Length).TrimStart('\', '/')
            } else {
                $relativePath = $fullPath
            }
            $relativePath -replace '\\', '/'
        }
    }
    return $rFiles
}

function Get-FunctionSignature {
    param([string]$Content, [string]$FunctionName)
    
    # Extract function signature (name + parameters) instead of full body
    $lines = $Content -split '\r?\n'
    $functionLines = @()
    $foundFunction = $false
    $parenCount = 0
    
    for ($i = 0; $i -lt $lines.Count; $i++) {
        $line = $lines[$i]
        
        # Look for function start
        if ($line -match "^\s*$([regex]::Escape($FunctionName))\s*(<-|=)\s*function") {
            $foundFunction = $true
            $functionLines += $line
            $parenCount = ($line.ToCharArray() | Where-Object { $_ -eq '(' }).Count - ($line.ToCharArray() | Where-Object { $_ -eq ')' }).Count
            
            # If parameters are complete on same line
            if ($parenCount -eq 0) {
                break
            }
            continue
        }
        
        if ($foundFunction) {
            $functionLines += $line
            $parenCount += ($line.ToCharArray() | Where-Object { $_ -eq '(' }).Count - ($line.ToCharArray() | Where-Object { $_ -eq ')' }).Count
            
            # If parentheses are balanced, function signature is complete
            if ($parenCount -eq 0) {
                break
            }
        }
    }
    
    if ($foundFunction) {
        $signature = $functionLines -join ' '
        # Clean up extra whitespace
        $signature = $signature -replace '\s+', ' '
        return $signature.Trim()
    }
    
    return ""
}

function Get-S3Methods {
    param([string]$Content)
    
    $s3Methods = @{}
    $lines = $Content -split '\r?\n'
    
    foreach ($line in $lines) {
        # Look for S3 method definitions: method.class <- function
        if ($line -match '^\s*([a-zA-Z_][a-zA-Z0-9_.]*\.[a-zA-Z_][a-zA-Z0-9_.]*)\s*(<-|=)\s*function') {
            $methodName = $matches[1]
            if ($methodName.Contains('.')) {
                $parts = $methodName -split '\.'
                $method = $parts[0]
                $class = $parts[1..($parts.Length-1)] -join '.'
                
                if (!$s3Methods.ContainsKey($method)) {
                    $s3Methods[$method] = @()
                }
                $s3Methods[$method] += $class
            }
        }
    }
    
    return $s3Methods
}

function Extract-FunctionArguments {
    param([string]$Signature)
    
    if (!$Signature) { return @() }
    
    # Extract content between parentheses
    if ($Signature -match 'function\s*\(([^)]*)\)') {
        $paramString = $matches[1].Trim()
        if (!$paramString) { return @() }
        
        # Split by comma and extract parameter names
        $params = $paramString -split ','
        $arguments = @()
        
        foreach ($param in $params) {
            $param = $param.Trim()
            # Extract parameter name (before = if there's a default value)
            if ($param -match '^([a-zA-Z_][a-zA-Z0-9_.]*)') {
                $arguments += $matches[1]
            }
        }
        
        return $arguments
    }
    
    return @()
}

function Analyze-FunctionSignature {
    param([string]$Signature, [string]$FunctionName, [hashtable]$AllFunctions, [string[]]$ExportedFunctions)
    
    $result = @{
        arguments = @()
        signature = $Signature
        is_s3_method = $false
        s3_class = ""
        s3_method = ""
    }
    
    if (!$Signature) { return $result }
    
    # Extract function arguments
    $result.arguments = Extract-FunctionArguments -Signature $Signature
    
    # Check if this is an S3 method
    if ($FunctionName.Contains('.')) {
        $parts = $FunctionName -split '\.'
        if ($parts.Length -ge 2) {
            $result.is_s3_method = $true
            $result.s3_method = $parts[0]
            $result.s3_class = $parts[1..($parts.Length-1)] -join '.'
        }
    }
    
    return $result
}

function Find-CircularDependency {
    param([string]$FunctionName, [hashtable]$DetailedDeps, [hashtable]$Functions, [string[]]$Visited, [string[]]$Path)
    
    if ($FunctionName -in $Path) {
        # Found a cycle
        $cycleStart = [array]::IndexOf($Path, $FunctionName)
        $cycle = $Path[$cycleStart..($Path.Length-1)] + $FunctionName
        return ($cycle -join ' -> ')
    }
    
    if ($FunctionName -in $Visited) {
        return $null
    }
    
    $Visited += $FunctionName
    $Path += $FunctionName
    
    # Get the file containing this function
    if (!$Functions.ContainsKey($FunctionName)) {
        return $null
    }
    
    $file = $Functions[$FunctionName]
    if (!$DetailedDeps.ContainsKey($file) -or !$DetailedDeps[$file].ContainsKey($FunctionName)) {
        return $null
    }
    
    # Check internal dependencies
    $deps = $DetailedDeps[$file][$FunctionName].internal_deps
    foreach ($dep in $deps) {
        $result = Find-CircularDependency -FunctionName $dep -DetailedDeps $DetailedDeps -Functions $Functions -Visited $Visited -Path $Path
        if ($result) {
            return $result
        }
    }
    
    return $null
}

function Get-FunctionDefinitions {
    param([string[]]$Files, [string[]]$ExportedFunctions)
    
    $functions = @{}
    $definitions = @{}
    $function_signatures = @{}
    $s3_methods = @{}
    $s3_classes = @()
    
    # Analyze each file for functions and S3 methods
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
        $fileSignatures = @{}
        
        # Function definition patterns
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
                    
                    # Extract function signature
                    $signature = Get-FunctionSignature -Content $content -FunctionName $cleanName
                    if ($signature) {
                        $fileSignatures[$cleanName] = $signature
                        $function_signatures[$cleanName] = $signature
                    }
                }
            }
        }
        
        # Extract S3 methods from this file
        $fileS3Methods = Get-S3Methods -Content $content
        foreach ($method in $fileS3Methods.Keys) {
            if (!$s3_methods.ContainsKey($method)) {
                $s3_methods[$method] = @()
            }
            $s3_methods[$method] += $fileS3Methods[$method]
            # Track unique classes
            foreach ($class in $fileS3Methods[$method]) {
                if ($class -notin $s3_classes) {
                    $s3_classes += $class
                }
            }
        }
        
        if ($fileFunctions.Count -gt 0) {
            $definitions[$file] = $fileFunctions
            Write-Host "Found $($fileFunctions.Count) functions: $($fileFunctions -join ', ')" -ForegroundColor Green
        }
        
        if ($fileS3Methods.Count -gt 0) {
            $methodSummary = $fileS3Methods.Keys | ForEach-Object { "$_(" + ($fileS3Methods[$_] -join ', ') + ")" }
            Write-Host "Found S3 methods: $($methodSummary -join ', ')" -ForegroundColor Magenta
        }
    }
    
    # Second pass: analyze function signatures for detailed information
    Write-Host "Step 1b: Analyzing function signatures..." -ForegroundColor Yellow
    $detailed_analysis = @{}
    
    foreach ($funcName in $function_signatures.Keys) {
        $signature = $function_signatures[$funcName]
        $analysis = Analyze-FunctionSignature -Signature $signature -FunctionName $funcName -AllFunctions $functions -ExportedFunctions $ExportedFunctions
        $detailed_analysis[$funcName] = $analysis
    }
    
    return @{
        functions = $functions
        definitions = $definitions  
        function_signatures = $function_signatures
        detailed_analysis = $detailed_analysis
        s3_methods = $s3_methods
        s3_classes = $s3_classes | Sort-Object -Unique
    }
}

function New-PackageDependencyMap {
    Write-Host "Creating package-wide function dependency map..." -ForegroundColor Cyan
    
    $startTime = Get-Date
    
    # Get exported functions
    $exportedFunctions = Get-ExportedFunctions
    
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
            exported_functions = $exportedFunctions.Count
        }
        functions = @{}
        definitions = @{}
        dependencies = @{}
        exported = $exportedFunctions
    }
    
    # Discover all R files
    $rFiles = Get-AllRFiles
    $dependencyMap.metadata.total_files = $rFiles.Count
    
    Write-Host "Analyzing $($rFiles.Count) R files..." -ForegroundColor Green
    
    # Find function definitions, signatures, and S3 methods
    Write-Host "Step 1: Discovering function definitions..." -ForegroundColor Yellow
    $funcResults = Get-FunctionDefinitions -Files $rFiles -ExportedFunctions $exportedFunctions
    $dependencyMap.functions = $funcResults.functions
    $dependencyMap.definitions = $funcResults.definitions
    $dependencyMap.function_signatures = $funcResults.function_signatures
    $dependencyMap.detailed_analysis = $funcResults.detailed_analysis
    $dependencyMap.s3_methods = $funcResults.s3_methods
    $dependencyMap.s3_classes = $funcResults.s3_classes
    $dependencyMap.metadata.total_functions = $funcResults.functions.Count
    $dependencyMap.metadata.s3_methods_count = $funcResults.s3_methods.Count
    $dependencyMap.metadata.s3_classes_count = $funcResults.s3_classes.Count
    
    # Save results
    Write-Host "Step 2: Saving dependency map..." -ForegroundColor Yellow
    
    # Save main dependency map
    $dependencyMap | ConvertTo-Json -Depth 10 | Out-File -FilePath $CACHE_FILE -Encoding UTF8
    
    # Save metadata separately
    $dependencyMap.metadata | ConvertTo-Json -Depth 5 | Out-File -FilePath $METADATA_FILE -Encoding UTF8
    
    # Classify functions as user-facing or internal
    $userFacingFunctions = @()
    $internalFunctions = @()
    
    foreach ($funcName in $dependencyMap.functions.Keys) {
        if ($funcName -in $exportedFunctions) {
            $userFacingFunctions += $funcName
        } else {
            $internalFunctions += $funcName
        }
    }

    # Create markdown visualization
    $markdown = @"
# Package Dependency Map

**Generated:** $($dependencyMap.metadata.generated)  
**Package:** $($dependencyMap.metadata.package_name) v$($dependencyMap.metadata.version)  
**Commit:** $($dependencyMap.metadata.commit_hash)  

## Summary

- **Total Files:** $($dependencyMap.metadata.total_files)
- **Total Functions:** $($dependencyMap.metadata.total_functions)
- **Exported Functions:** $($dependencyMap.metadata.exported_functions)
- **Internal Functions:** $($internalFunctions.Count)
- **S3 Methods:** $($dependencyMap.metadata.s3_methods_count)
- **S3 Classes:** $($dependencyMap.metadata.s3_classes_count)

## User Interface (Exported Functions)

### Main Entry Points
"@

    # Group exported functions by category
    $mainFunctions = $userFacingFunctions | Where-Object { $_ -in @('mvgam', 'sim_mvgam', 'series_to_mvgam') }
    $plotFunctions = $userFacingFunctions | Where-Object { $_ -like 'plot_*' }
    $trendFunctions = $userFacingFunctions | Where-Object { $_ -in @('RW', 'AR', 'VAR', 'CAR', 'GP', 'PW', 'ZMVN') }
    $analysisFunctions = $userFacingFunctions | Where-Object { $_ -in @('lfo_cv', 'loo', 'ppc', 'pp_check', 'conditional_effects', 'fevd', 'irf', 'stability') }
    
    if ($mainFunctions.Count -gt 0) {
        $markdown += "`n- **Core Functions**: " + ($mainFunctions -join ', ')
    }
    if ($trendFunctions.Count -gt 0) {
        $markdown += "`n- **Trend Types**: " + ($trendFunctions -join ', ')
    }
    if ($analysisFunctions.Count -gt 0) {
        $markdown += "`n- **Analysis Tools**: " + ($analysisFunctions -join ', ')
    }
    if ($plotFunctions.Count -gt 0) {
        $markdown += "`n- **Plotting Functions**: " + ($plotFunctions -join ', ')
    }

    # Add S3 system information
    $markdown += "`n`n## S3 Object System`n"
    
    # S3 Classes
    if ($dependencyMap.s3_classes.Count -gt 0) {
        $markdown += "`n### S3 Classes`n"
        $markdown += "- " + ($dependencyMap.s3_classes -join ', ') + "`n"
    }
    
    # S3 Methods
    $markdown += "`n### S3 Methods`n"
    foreach ($method in ($dependencyMap.s3_methods.Keys | Sort-Object)) {
        $classes = $dependencyMap.s3_methods[$method] | Sort-Object -Unique
        $markdown += "- **$method()**: " + ($classes -join ', ') + "`n"
    }
    
    # Key function signatures
    $markdown += "`n## Key Function Signatures`n"
    
    $keyFunctions = @('mvgam', 'plot.mvgam', 'conditional_effects.mvgam', 'sim_mvgam', 'lfo_cv.mvgam')
    foreach ($keyFunc in $keyFunctions) {
        if ($dependencyMap.function_signatures.ContainsKey($keyFunc)) {
            $signature = $dependencyMap.function_signatures[$keyFunc]
            $markdown += "`n### ``$keyFunc``:`n"
            $markdown += "``````r`n$signature`n```````n"
            
            if ($dependencyMap.detailed_analysis.ContainsKey($keyFunc)) {
                $analysis = $dependencyMap.detailed_analysis[$keyFunc]
                if ($analysis.arguments.Count -gt 0) {
                    $markdown += "**Arguments**: " + ($analysis.arguments -join ', ') + "`n"
                }
                if ($analysis.is_s3_method) {
                    $markdown += "**S3 Method**: $($analysis.s3_method) for class $($analysis.s3_class)`n"
                }
            }
        }
    }

    $markdown += "`n`n## Function Definitions by File`n"

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
        exit 2  # Success with output
    } else {
        Write-Host "Hook triggered for event: $Event (no action taken)" -ForegroundColor Gray
        exit 0  # Silent success
    }
} catch {
    Write-Host "Error in dependency mapper: $_" -ForegroundColor Red
    Write-Host "Stack trace: $($_.ScriptStackTrace)" -ForegroundColor Red
    exit 1  # Error
}