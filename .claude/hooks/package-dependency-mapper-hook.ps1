# Package-Wide Function Dependency Mapper Hook - Optimized Version
param([string]$Event, [string]$CommitHash, [string]$CommitMessage)

# Configuration
$CACHE_FILE = ".architecture/package-dependency-map.json"
$METADATA_FILE = "architecture/dependency-metadata.json"
$VISUALIZATION_FILE = "architecture/dependency-graph.md"
$FILE_CACHE_DIR = ".architecture/file-cache"

# Suppress output - all Write-Host calls removed or redirected to Write-Debug
$DebugPreference = 'SilentlyContinue'

# Pre-compile regex patterns for better performance
$Script:CompiledRegex = @{
    FunctionDef1 = [regex]::new('^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*function\s*\(', [System.Text.RegularExpressions.RegexOptions]::Multiline -bor [System.Text.RegularExpressions.RegexOptions]::Compiled)
    FunctionDef2 = [regex]::new('^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*=\s*function\s*\(', [System.Text.RegularExpressions.RegexOptions]::Multiline -bor [System.Text.RegularExpressions.RegexOptions]::Compiled)
    S3Method = [regex]::new('^\s*([a-zA-Z_][a-zA-Z0-9_.]*\.[a-zA-Z_][a-zA-Z0-9_.]*)\s*(<-|=)\s*function', [System.Text.RegularExpressions.RegexOptions]::Multiline -bor [System.Text.RegularExpressions.RegexOptions]::Compiled)
    FunctionArgs = [regex]::new('function\s*\(([^)]*)\)', [System.Text.RegularExpressions.RegexOptions]::Compiled)
    ParamName = [regex]::new('^([a-zA-Z_][a-zA-Z0-9_.]*)', [System.Text.RegularExpressions.RegexOptions]::Compiled)
    ImportsSection = [regex]::new('Imports:\s*([^:\r\n]+(?:\r?\n\s+[^:\r\n]+)*)', [System.Text.RegularExpressions.RegexOptions]::Compiled)
    DependsSection = [regex]::new('Depends:\s*([^:\r\n]+(?:\r?\n\s+[^:\r\n]+)*)', [System.Text.RegularExpressions.RegexOptions]::Compiled)
}

# Ensure cache directory exists
if (!(Test-Path $FILE_CACHE_DIR)) {
    New-Item -ItemType Directory -Path $FILE_CACHE_DIR -Force | Out-Null
}

# Ensure architecture directory exists
$architectureDir = Split-Path $CACHE_FILE -Parent
if (!(Test-Path $architectureDir)) {
    New-Item -ItemType Directory -Path $architectureDir -Force | Out-Null
}

function Get-FileHash-Fast {
    param([string]$FilePath)
    
    if (!(Test-Path $FilePath)) { return $null }
    
    $fileInfo = Get-Item $FilePath
    return "$($fileInfo.Length):$($fileInfo.LastWriteTime.Ticks)"
}

function Get-ChangedFiles {
    param([string[]]$AllFiles)
    
    $changedFiles = @()
    $cacheMetaFile = Join-Path $FILE_CACHE_DIR "file-metadata.json"
    $lastFileHashes = @{}
    
    if (Test-Path $cacheMetaFile) {
        try {
            $lastFileHashes = Get-Content $cacheMetaFile | ConvertFrom-Json -AsHashtable -ErrorAction SilentlyContinue
        } catch {
            # If cache is corrupted, process all files
            $lastFileHashes = @{}
        }
    }
    
    foreach ($file in $AllFiles) {
        $currentHash = Get-FileHash-Fast -FilePath $file
        $lastHash = $lastFileHashes[$file]
        
        if ($currentHash -ne $lastHash) {
            $changedFiles += $file
            $lastFileHashes[$file] = $currentHash
        }
    }
    
    # Save updated file hashes
    $lastFileHashes | ConvertTo-Json -Depth 2 | Out-File -FilePath $cacheMetaFile -Encoding UTF8
    
    return $changedFiles
}

function Get-CachedFileAnalysis {
    param([string]$FilePath)
    
    $cacheFile = Join-Path $FILE_CACHE_DIR ([System.IO.Path]::GetFileNameWithoutExtension($FilePath) + ".json")
    
    if (Test-Path $cacheFile) {
        try {
            return Get-Content $cacheFile | ConvertFrom-Json -AsHashtable -ErrorAction SilentlyContinue
        } catch {
            return $null
        }
    }
    
    return $null
}

function Set-CachedFileAnalysis {
    param([string]$FilePath, [hashtable]$Analysis)
    
    $cacheFile = Join-Path $FILE_CACHE_DIR ([System.IO.Path]::GetFileNameWithoutExtension($FilePath) + ".json")
    $Analysis | ConvertTo-Json -Depth 5 | Out-File -FilePath $cacheFile -Encoding UTF8
}

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

function Get-ExternalDependencies {
    $dependencies = @()
    if (Test-Path "DESCRIPTION") {
        $content = Get-Content "DESCRIPTION" -Raw
        
        # Extract Imports using pre-compiled regex
        $importsMatch = $Script:CompiledRegex.ImportsSection.Match($content)
        if ($importsMatch.Success) {
            $imports = $importsMatch.Groups[1].Value -replace '\s+', ' '
            $dependencies += ($imports -split ',\s*' | ForEach-Object { 
                ($_ -split '\s*\(')[0].Trim() 
            } | Where-Object { $_ -and $_ -ne '' })
        }
        
        # Extract Depends using pre-compiled regex
        $dependsMatch = $Script:CompiledRegex.DependsSection.Match($content)
        if ($dependsMatch.Success) {
            $depends = $dependsMatch.Groups[1].Value -replace '\s+', ' '
            $dependencies += ($depends -split ',\s*' | ForEach-Object { 
                ($_ -split '\s*\(')[0].Trim() 
            } | Where-Object { $_ -and $_ -ne 'R' -and $_ -ne '' })
        }
    }
    return $dependencies | Sort-Object -Unique
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

function Get-FunctionCalls {
    param([string]$Content, [string]$FunctionName, [string[]]$AllFunctionNames)
    
    $calls = @()
    
    # Extract the function body
    $lines = $Content -split '\r?\n'
    $inFunction = $false
    $braceCount = 0
    $functionBody = ""
    
    for ($i = 0; $i -lt $lines.Count; $i++) {
        $line = $lines[$i]
        
        if ($line -match "^\s*$([regex]::Escape($FunctionName))\s*(<-|=)\s*function") {
            $inFunction = $true
            # Count braces on the same line
            $braceCount = ($line.ToCharArray() | Where-Object { $_ -eq '{' }).Count - ($line.ToCharArray() | Where-Object { $_ -eq '}' }).Count
            $functionBody += $line + "`n"
            continue
        }
        
        if ($inFunction) {
            $functionBody += $line + "`n"
            $braceCount += ($line.ToCharArray() | Where-Object { $_ -eq '{' }).Count - ($line.ToCharArray() | Where-Object { $_ -eq '}' }).Count
            
            if ($braceCount -eq 0 -and $line -match '}') {
                break
            }
        }
    }
    
    # Look for function calls in the body
    foreach ($func in $AllFunctionNames) {
        if ($func -ne $FunctionName) {  # Don't count self-references
            # Look for function calls: func( or func ( but not inside quotes
            $pattern = "(?<!['\`"`"])$([regex]::Escape($func))\s*\("
            if ($functionBody -match $pattern) {
                $calls += $func
            }
        }
    }
    
    return $calls | Sort-Object -Unique
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
    
    # Extract content between parentheses using pre-compiled regex
    $argsMatch = $Script:CompiledRegex.FunctionArgs.Match($Signature)
    if ($argsMatch.Success) {
        $paramString = $argsMatch.Groups[1].Value.Trim()
        if (!$paramString) { return @() }
        
        # Split by comma and extract parameter names
        $params = $paramString -split ','
        $arguments = @()
        
        foreach ($param in $params) {
            $param = $param.Trim()
            # Extract parameter name (before = if there's a default value) using pre-compiled regex
            $paramMatch = $Script:CompiledRegex.ParamName.Match($param)
            if ($paramMatch.Success) {
                $arguments += $paramMatch.Groups[1].Value
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

function Get-FilePurpose {
    param([string]$FilePath)
    
    $fileName = Split-Path -Leaf $FilePath
    $baseName = [System.IO.Path]::GetFileNameWithoutExtension($fileName)
    
    # Common patterns for R package files
    $purposes = @{
        'zzz' = 'Package startup and attachment'
        'RcppExports' = 'Rcpp compiled function exports'
        'data' = 'Data documentation and loading'
        'utils' = 'Utility functions'
        'helpers' = 'Helper functions'
        'plot' = 'Plotting and visualization'
        'print' = 'Print methods for objects'
        'summary' = 'Summary methods'
        'predict' = 'Prediction methods'
        'fit' = 'Model fitting'
        'stan' = 'Stan model integration'
        'brms' = 'brms integration'
        'validation' = 'Input validation and checks'
        'priors' = 'Prior specification'
        'diagnostic' = 'Model diagnostics'
        'forecast' = 'Forecasting functions'
        'trend' = 'Trend modeling'
        'smooth' = 'Smoothing functions'
        'simulate' = 'Simulation functions'
        'residual' = 'Residual analysis'
        'effects' = 'Effect calculations'
        'marginal' = 'Marginal effects'
        'conditional' = 'Conditional effects'
        'core' = 'Core package functionality'
        'class' = 'Class definitions and methods'
        'methods' = 'Method implementations'
        'export' = 'Exported functions'
        'internal' = 'Internal functions'
    }
    
    foreach ($pattern in $purposes.Keys) {
        if ($baseName -match $pattern) {
            return $purposes[$pattern]
        }
    }
    
    # Try to infer from file name
    if ($baseName -match '^(.+?)[-_]') {
        $prefix = $matches[1]
        switch ($prefix) {
            'plot' { return 'Plotting functions' }
            'get' { return 'Getter/accessor functions' }
            'set' { return 'Setter/mutator functions' }
            'check' { return 'Validation and checking' }
            'make' { return 'Constructor functions' }
            'build' { return 'Building/assembly functions' }
            'extract' { return 'Data extraction' }
            'generate' { return 'Generation functions' }
            'compute' { return 'Computational functions' }
            'calculate' { return 'Calculation functions' }
            default { return '' }
        }
    }
    
    return ''
}


function Get-FunctionDefinitions {
    param([string[]]$Files, [string[]]$ExportedFunctions, [string[]]$ChangedFiles)
    
    $functions = @{}
    $definitions = @{}
    $function_signatures = @{}
    $function_dependencies = @{}
    $s3_methods = @{}
    $s3_classes = @()
    $file_purposes = @{}
    
    Write-Debug "Processing $($Files.Count) files ($($ChangedFiles.Count) changed)..."
    
    # Process files with caching optimization
    $fileResults = @()
    foreach ($filePath in $Files) {
        Write-Debug "Processing $filePath..."
        
        if (!(Test-Path $filePath)) {
            continue
        }
        
        # Skip cache if file has changed
        if ($filePath -in $ChangedFiles) {
            # Force re-analysis by removing cache
            $cacheFile = Join-Path $FILE_CACHE_DIR ([System.IO.Path]::GetFileNameWithoutExtension($filePath) + ".json")
            if (Test-Path $cacheFile) {
                Remove-Item $cacheFile -Force
            }
        }
        
        # Check cache first
        $cached = Get-CachedFileAnalysis -FilePath $filePath
        if ($cached -and ($filePath -notin $ChangedFiles)) {
            Write-Debug "Using cached analysis for $filePath"
            $fileResults += $cached
            continue
        }
        
        $content = Get-Content $filePath -Raw -ErrorAction SilentlyContinue
        if (!$content) {
            continue
        }
        
        $result = @{
            file = $filePath
            functions = @()
            function_signatures = @{}
            function_dependencies = @{}
            s3_methods = @{}
            purpose = ""
        }
        
        # Get file purpose
        $purpose = Get-FilePurpose -FilePath $filePath
        if ($purpose) {
            $result.purpose = $purpose
        }
        
        # Find function definitions using pre-compiled regex
        $fileFunctions = @()
        
        $matches1 = $Script:CompiledRegex.FunctionDef1.Matches($content)
        $matches2 = $Script:CompiledRegex.FunctionDef2.Matches($content)
        
        foreach ($match in ($matches1 + $matches2)) {
            $functionName = $match.Groups[1].Value.Trim()
            if ($functionName -and $functionName -ne "") {
                $fileFunctions += $functionName
            }
        }
        
        $result.functions = $fileFunctions
        
        # Analyze each function in this file
        foreach ($funcName in $fileFunctions) {
            # Extract signature
            $signature = Get-FunctionSignature -Content $content -FunctionName $funcName
            if ($signature) {
                $result.function_signatures[$funcName] = $signature
            }
        }
        
        # Extract S3 methods using pre-compiled regex
        $s3Matches = $Script:CompiledRegex.S3Method.Matches($content)
        foreach ($match in $s3Matches) {
            $methodName = $match.Groups[1].Value
            if ($methodName.Contains('.')) {
                $parts = $methodName -split '\.'
                $method = $parts[0]
                $class = $parts[1..($parts.Length-1)] -join '.'
                
                if (!$result.s3_methods.ContainsKey($method)) {
                    $result.s3_methods[$method] = @()
                }
                $result.s3_methods[$method] += $class
            }
        }
        
        # Cache the result
        Set-CachedFileAnalysis -FilePath $filePath -Analysis $result
        
        $fileResults += $result
    }
    
    # Combine results from parallel processing
    foreach ($fileResult in $fileResults) {
        if (!$fileResult) { continue }
        
        $filePath = $fileResult.file
        $definitions[$filePath] = $fileResult.functions
        
        if ($fileResult.purpose) {
            $file_purposes[$filePath] = $fileResult.purpose
        }
        
        # Add functions to global function map
        foreach ($funcName in $fileResult.functions) {
            $functions[$funcName] = $filePath
        }
        
        # Add function signatures
        foreach ($funcName in $fileResult.function_signatures.Keys) {
            $function_signatures[$funcName] = $fileResult.function_signatures[$funcName]
        }
        
        # Add S3 methods
        foreach ($method in $fileResult.s3_methods.Keys) {
            if (!$s3_methods.ContainsKey($method)) {
                $s3_methods[$method] = @()
            }
            $s3_methods[$method] += $fileResult.s3_methods[$method]
            foreach ($class in $fileResult.s3_methods[$method]) {
                if ($class -notin $s3_classes) {
                    $s3_classes += $class
                }
            }
        }
    }
    
    # Second pass: analyze function dependencies (only for changed files for efficiency)
    foreach ($filePath in $ChangedFiles) {
        if (!(Test-Path $filePath) -or !$definitions.ContainsKey($filePath)) {
            continue
        }
        
        $content = Get-Content $filePath -Raw -ErrorAction SilentlyContinue
        if (!$content) {
            continue
        }
        
        foreach ($funcName in $definitions[$filePath]) {
            # Extract function calls (dependencies)
            $calls = Get-FunctionCalls -Content $content -FunctionName $funcName -AllFunctionNames $functions.Keys
            if ($calls.Count -gt 0) {
                $function_dependencies[$funcName] = $calls
            }
        }
    }
    
    # Third pass: analyze function signatures for detailed information
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
        function_dependencies = $function_dependencies
        detailed_analysis = $detailed_analysis
        s3_methods = $s3_methods
        s3_classes = $s3_classes | Sort-Object -Unique
        file_purposes = $file_purposes
    }
}

function New-PackageDependencyMap {
    Write-Debug "Creating optimized package-wide function dependency map..."
    
    $startTime = Get-Date
    
    # Get exported functions and external dependencies
    $exportedFunctions = Get-ExportedFunctions
    $externalDeps = Get-ExternalDependencies
    
    # Discover all R files
    $rFiles = Get-AllRFiles
    Write-Debug "Found $($rFiles.Count) R files"
    
    # Determine which files have changed since last run
    $changedFiles = Get-ChangedFiles -AllFiles $rFiles
    Write-Debug "Changed files: $($changedFiles.Count) of $($rFiles.Count)"
    
    # Load existing analysis if available
    $existingAnalysis = @{}
    if ((Test-Path $CACHE_FILE) -and ($changedFiles.Count -lt $rFiles.Count)) {
        try {
            $existingData = Get-Content $CACHE_FILE | ConvertFrom-Json -AsHashtable
            $existingAnalysis = $existingData
            Write-Debug "Loaded existing analysis"
        } catch {
            Write-Debug "Could not load existing analysis, starting fresh"
            $existingAnalysis = @{}
        }
    }
    
    # Initialize the dependency map structure
    $dependencyMap = @{
        metadata = @{
            generated = $startTime.ToString("yyyy-MM-dd HH:mm:ss")
            commit_hash = $CommitHash
            commit_message = $CommitMessage
            package_name = Get-PackageName
            version = Get-PackageVersion
            total_files = $rFiles.Count
            total_functions = 0
            exported_functions = $exportedFunctions.Count
            external_dependencies = $externalDeps
            changed_files = $changedFiles.Count
            cached_files = ($rFiles.Count - $changedFiles.Count)
        }
        functions = @{}
        definitions = @{}
        dependencies = @{}
        exported = $exportedFunctions
    }
    
    Write-Debug "Analyzing $($rFiles.Count) R files ($($changedFiles.Count) changed, $($rFiles.Count - $changedFiles.Count) cached)..."
    
    # Find function definitions, signatures, dependencies, and S3 methods with optimizations
    $funcResults = Get-FunctionDefinitions -Files $rFiles -ExportedFunctions $exportedFunctions -ChangedFiles $changedFiles
    
    # Merge with existing analysis for unchanged files
    if ($existingAnalysis.Count -gt 0) {
        # Copy over function dependencies for unchanged files
        if ($existingAnalysis.ContainsKey('function_dependencies')) {
            foreach ($funcName in $existingAnalysis.function_dependencies.Keys) {
                $funcFile = $funcResults.functions[$funcName]
                if ($funcFile -and ($funcFile -notin $changedFiles)) {
                    $funcResults.function_dependencies[$funcName] = $existingAnalysis.function_dependencies[$funcName]
                }
            }
        }
    }
    
    $dependencyMap.functions = $funcResults.functions
    $dependencyMap.definitions = $funcResults.definitions
    $dependencyMap.function_signatures = $funcResults.function_signatures
    $dependencyMap.function_dependencies = $funcResults.function_dependencies
    $dependencyMap.detailed_analysis = $funcResults.detailed_analysis
    $dependencyMap.s3_methods = $funcResults.s3_methods
    $dependencyMap.s3_classes = $funcResults.s3_classes
    $dependencyMap.file_purposes = $funcResults.file_purposes
    $dependencyMap.metadata.total_functions = $funcResults.functions.Count
    $dependencyMap.metadata.s3_methods_count = $funcResults.s3_methods.Count
    $dependencyMap.metadata.s3_classes_count = $funcResults.s3_classes.Count
    
    # Save results
    Write-Debug "Saving dependency map..."
    
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
**Commit:** $($CommitHash.Substring(0, [Math]::Min(7, $CommitHash.Length)))  

## Summary

- **Total Files:** $($dependencyMap.metadata.total_files)
- **Total Functions:** $($dependencyMap.metadata.total_functions)
- **Exported Functions:** $($dependencyMap.metadata.exported_functions)
- **Internal Functions:** $($internalFunctions.Count)
- **S3 Methods:** $($dependencyMap.metadata.s3_methods_count)
- **S3 Classes:** $($dependencyMap.metadata.s3_classes_count)

## External Dependencies

"@
    
    if ($externalDeps.Count -gt 0) {
        foreach ($dep in $externalDeps) {
            $markdown += "- $dep`n"
        }
    } else {
        $markdown += "- None (base R only)`n"
    }
    
    $markdown += @"

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
    
    # S3 Methods (limit to most important)
    $markdown += "`n### Key S3 Methods`n"
    $importantMethods = @('plot', 'print', 'summary', 'predict', 'residuals', 'fitted')
    foreach ($method in ($dependencyMap.s3_methods.Keys | Sort-Object)) {
        if ($method -in $importantMethods -or $dependencyMap.s3_methods[$method].Count -ge 2) {
            $classes = $dependencyMap.s3_methods[$method] | Sort-Object -Unique
            $markdown += "- **$method()**: " + ($classes -join ', ') + "`n"
        }
    }
    
    # Core data structures
    $markdown += "`n## Core Data Structures`n"
    $markdown += "- **mvgam object**: Fitted model with data, parameters, and predictions`n"
    $markdown += "- **mvgam_prefit**: Pre-compilation model structure`n"
    $markdown += "- **mvgam_forecast**: Forecast results with uncertainty`n"
    $markdown += "- **trend_param**: Trend model specifications`n"
    
    # Key function dependencies (limit to most important)
    $markdown += "`n## Key Function Dependencies`n"
    
    $keyDeps = @('mvgam', 'plot.mvgam', 'sim_mvgam', 'lfo_cv.mvgam')
    foreach ($func in $keyDeps) {
        if ($dependencyMap.function_dependencies.ContainsKey($func)) {
            $deps = $dependencyMap.function_dependencies[$func]
            if ($deps.Count -gt 0) {
                # Limit to first 5 dependencies for brevity
                $depList = if ($deps.Count -gt 5) { 
                    ($deps[0..4] -join ', ') + ", ..."
                } else {
                    $deps -join ', '
                }
                $markdown += "- **$func()** calls: $depList`n"
            }
        }
    }
    
    # Key function signatures (reduced set)
    $markdown += "`n## Key Function Signatures`n"
    
    $keyFunctions = @('mvgam', 'sim_mvgam')
    foreach ($keyFunc in $keyFunctions) {
        if ($dependencyMap.function_signatures.ContainsKey($keyFunc)) {
            $signature = $dependencyMap.function_signatures[$keyFunc]
            # Truncate long signatures
            if ($signature.Length -gt 150) {
                $signature = $signature.Substring(0, 147) + "..."
            }
            $markdown += "`n### ``$keyFunc``:`n"
            $markdown += "``````r`n$signature`n```````n"
            
            if ($dependencyMap.detailed_analysis.ContainsKey($keyFunc)) {
                $analysis = $dependencyMap.detailed_analysis[$keyFunc]
                if ($analysis.arguments.Count -gt 0) {
                    # Show first 6 arguments
                    $argList = if ($analysis.arguments.Count -gt 6) {
                        ($analysis.arguments[0..5] -join ', ') + ", ..."
                    } else {
                        $analysis.arguments -join ', '
                    }
                    $markdown += "**Arguments**: $argList`n"
                }
            }
        }
    }

    # File organization (with purposes)
    $markdown += "`n## File Organization`n"
    
    # Group files by category
    $coreFiles = @('R/mvgam_core.R', 'R/mvgam.R', 'R/sim_mvgam.R')
    $validationFiles = $dependencyMap.definitions.Keys | Where-Object { $_ -match 'validat|check' }
    $plotFiles = $dependencyMap.definitions.Keys | Where-Object { $_ -match 'plot' }
    $stanFiles = $dependencyMap.definitions.Keys | Where-Object { $_ -match 'stan|brms' }
    
    foreach ($file in $coreFiles) {
        if ($dependencyMap.definitions.ContainsKey($file)) {
            $purpose = if ($dependencyMap.file_purposes.ContainsKey($file)) { 
                ": " + $dependencyMap.file_purposes[$file] 
            } else { "" }
            $markdown += "- **$file**$purpose`n"
        }
    }
    
    if ($validationFiles.Count -gt 0) {
        $markdown += "- **Validation**: " + (($validationFiles | Select-Object -First 3) -join ', ')
        if ($validationFiles.Count -gt 3) { $markdown += ", ..." }
        $markdown += "`n"
    }
    
    if ($plotFiles.Count -gt 0) {
        $markdown += "- **Plotting**: " + (($plotFiles | Select-Object -First 3) -join ', ')
        if ($plotFiles.Count -gt 3) { $markdown += ", ..." }
        $markdown += "`n"
    }
    
    if ($stanFiles.Count -gt 0) {
        $markdown += "- **Stan/brms**: " + (($stanFiles | Select-Object -First 3) -join ', ')
        if ($stanFiles.Count -gt 3) { $markdown += ", ..." }
        $markdown += "`n"
    }
    
    $markdown | Out-File -FilePath $VISUALIZATION_FILE -Encoding UTF8
    
    $endTime = Get-Date
    $duration = ($endTime - $startTime).TotalSeconds
    
    Write-Debug "Package dependency mapping complete!"
    Write-Debug "Duration: $([math]::Round($duration, 2)) seconds"
}

# Main execution
try {
    if ($Event -eq "post_git_commit" -or $Event -eq "pre_git_commit" -or $Event -eq "test") {
        New-PackageDependencyMap
        exit 2  # Success with output (but no output shown due to Write-Debug)
    } else {
        Write-Debug "Hook triggered for event: $Event (no action taken)"
        exit 0  # Silent success
    }
} catch {
    # Only show errors, not regular output
    Write-Error "Error in dependency mapper: $_"
    Write-Error "Stack trace: $($_.ScriptStackTrace)"
    exit 1  # Error
}