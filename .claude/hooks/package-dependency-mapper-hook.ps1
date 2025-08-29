# Package-Wide Function Dependency Mapper Hook - Optimized Version
param([string]$Event, [string]$CommitHash, [string]$CommitMessage)

# Configuration
$CACHE_FILE = "architecture/package-dependency-map.json"
$METADATA_FILE = "architecture/dependency-metadata.json"
$VISUALIZATION_FILE = "architecture/dependency-graph.md"
$FILE_CACHE_DIR = "architecture/file-cache"

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

# Ensure architecture directory exists
$architectureDir = Split-Path $CACHE_FILE -Parent
if (!(Test-Path $architectureDir)) {
    New-Item -ItemType Directory -Path $architectureDir -Force | Out-Null
}

# Ensure cache directory exists
if (!(Test-Path $FILE_CACHE_DIR)) {
    New-Item -ItemType Directory -Path $FILE_CACHE_DIR -Force | Out-Null
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
            $jsonContent = Get-Content $cacheMetaFile -Raw
            $lastFileHashes = $jsonContent | ConvertFrom-Json
            # Convert PSCustomObject to hashtable for reliable lookups
            $hashTable = @{}
            $lastFileHashes.PSObject.Properties | ForEach-Object {
                $hashTable[$_.Name] = $_.Value
            }
            $lastFileHashes = $hashTable
        } catch {
            Write-Debug "Cache corrupted, processing all files"
            $lastFileHashes = @{}
        }
    }
    
    Write-Debug "Checking $($AllFiles.Count) files for changes..."
    
    foreach ($file in $AllFiles) {
        $currentHash = Get-FileHash-Fast -FilePath $file
        $lastHash = $lastFileHashes[$file]
        
        if (!$lastHash -or ($currentHash -ne $lastHash)) {
            $changedFiles += $file
            Write-Debug "$file changed (was: $lastHash, now: $currentHash)"
        } else {
            Write-Debug "$file unchanged ($currentHash)"
        }
        
        $lastFileHashes[$file] = $currentHash
    }
    
    Write-Debug "$($changedFiles.Count) changed files detected"
    
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
    
    # Extract the function body efficiently
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
    
    # Optimized approach: find all function calls with one regex, then filter
    # Look for any identifier followed by '(' - much faster than individual searches
    $callMatches = [regex]::Matches($functionBody, '([a-zA-Z_][a-zA-Z0-9_.]*)(\s*)\(', [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
    
    $foundCalls = @{}
    foreach ($match in $callMatches) {
        $funcCall = $match.Groups[1].Value
        if ($funcCall -in $AllFunctionNames -and $funcCall -ne $FunctionName) {
            $foundCalls[$funcCall] = $true
        }
    }
    
    return $foundCalls.Keys | Sort-Object
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
    
    # Second pass: analyze function dependencies (prioritize key integration files)
    $priorityFilesPattern = 'brms_integration|priors|stan_assembly|trend_system|mvgam_core|validations'
    $coreFilesPattern = 'mvgam\.R|sim_mvgam|plot\.mvgam|lfo_cv|series_to_mvgam'
    
    # Prioritize Stan/brms integration files + core/validation files first, then other core files
    $priorityFiles = $ChangedFiles | Where-Object { $_ -match $priorityFilesPattern }
    $coreFiles = $ChangedFiles | Where-Object { $_ -match $coreFilesPattern }
    $keyFiles = ($priorityFiles + $coreFiles) | Select-Object -First 10
    
    foreach ($filePath in $keyFiles) {
        if (!(Test-Path $filePath) -or !$definitions.ContainsKey($filePath)) {
            continue
        }
        
        Write-Debug "Analyzing dependencies in $filePath..."
        $content = Get-Content $filePath -Raw -ErrorAction SilentlyContinue
        if (!$content) {
            continue
        }
        
        # Prioritize functions based on file importance
        if ($filePath -match $priorityFilesPattern) {
            # For priority files (Stan/brms integration, core, validations), analyze many more functions
            $keyFunctions = $definitions[$filePath] | Where-Object { 
                $_ -in $exportedFunctions -or 
                $_ -match 'brms|stan|formula|prior|trend|code|data|assemble|validate|check|generate|build|mvgam|is\.|create|make|inject|merge|register|set_|get_|default_'
            } | Select-Object -First 15
        } else {
            # For other core files, expanded selection
            $keyFunctions = $definitions[$filePath] | Where-Object { 
                $_ -in $exportedFunctions -or $_ -match 'mvgam|plot|validate|check|create|build'
            } | Select-Object -First 5
        }
        
        foreach ($funcName in $keyFunctions) {
            # Extract function calls (dependencies) using optimized method
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
    
    # Enhanced function dependencies section for LLM agents
    $markdown += "`n## Function Dependencies & Architecture`n"
    
    # Prioritized Stan/brms integration functions
    $stanBrmsFunctions = @()
    foreach ($func in $dependencyMap.function_dependencies.Keys) {
        $file = $dependencyMap.functions[$func]
        if ($file -match 'brms_integration|priors|stan_assembly|trend_system|mvgam_core|validations') {
            $stanBrmsFunctions += $func
        }
    }
    
    if ($stanBrmsFunctions.Count -gt 0) {
        $markdown += "`n### Priority Integration Functions (Stan/brms/Core/Validation)`n"
        $topPriority = $stanBrmsFunctions | Select-Object -First 6
        foreach ($func in $topPriority) {
            $deps = $dependencyMap.function_dependencies[$func]
            $filePath = $dependencyMap.functions[$func]
            if ($deps.Count -gt 0) {
                $markdown += "- **$func()** (``$filePath``)`n"
                $markdown += "  - Internal calls: " + ($deps -join ', ') + "`n"
            }
        }
    }
    
    # Core workflow functions focusing on prior system, trend system, injection stanvars, stan_assembly
    $coreWorkflow = @(
        # Prior system
        'mvgam_formula', 'get_prior', 'set_prior', 'validate_priors', 'default_priors',
        # Trend system 
        'trend_param', 'register_trend_type', 'create_mvgam_trend', 'validate_trend_formula', 'trend_system',
        # Stan assembly system
        'stancode', 'standata', 'make_stancode', 'make_standata', 'assemble_stancode', 'build_standata',
        # Injection stanvars
        'inject_stanvar', 'add_stanvar', 'validate_stanvar', 'merge_stanvars',
        # Core entry points
        'mvgam', 'sim_mvgam'
    )
    $markdown += "`n### Core Workflow Functions (Prior/Trend/Stan Assembly Systems)`n"
    
    foreach ($func in $coreWorkflow) {
        if ($dependencyMap.function_dependencies.ContainsKey($func)) {
            $deps = $dependencyMap.function_dependencies[$func]
            $filePath = $dependencyMap.functions[$func]
            if ($deps.Count -gt 0) {
                $markdown += "- **$func()** (``$filePath``)`n"
                $markdown += "  - Internal calls: " + ($deps -join ', ') + "`n"
            }
        } elseif ($dependencyMap.functions.ContainsKey($func)) {
            $filePath = $dependencyMap.functions[$func]
            $markdown += "- **$func()** (``$filePath``) - No internal dependencies tracked`n"
        }
    }
    
    # Most connected internal functions (those with most dependencies)
    $markdown += "`n### Most Connected Internal Functions`n"
    $functionsByDepCount = @()
    foreach ($func in $dependencyMap.function_dependencies.Keys) {
        $depCount = $dependencyMap.function_dependencies[$func].Count
        if ($depCount -gt 3) {  # Functions with more than 3 dependencies
            $functionsByDepCount += @{
                name = $func
                deps = $dependencyMap.function_dependencies[$func]
                count = $depCount
                file = $dependencyMap.functions[$func]
            }
        }
    }
    
    $topConnected = $functionsByDepCount | Sort-Object { $_.count } -Descending | Select-Object -First 8
    foreach ($func in $topConnected) {
        $markdown += "- **$($func.name)()** (``$($func.file)``) - $($func.count) dependencies`n"
        $markdown += "  - Calls: " + ($func.deps -join ', ') + "`n"
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

    # Enhanced file organization for LLM agents
    $markdown += "`n## File Organization & Function Locations`n"
    
    # Create file categories with function counts and purposes
    $fileCategories = @{
        'Core' = @()
        'Plotting' = @()
        'Stan/Modeling' = @()
        'Validation' = @()
        'S3 Methods' = @()
        'Utilities' = @()
        'Data' = @()
        'Other' = @()
    }
    
    foreach ($file in $dependencyMap.definitions.Keys) {
        $functionCount = $dependencyMap.definitions[$file].Count
        $purpose = if ($dependencyMap.file_purposes.ContainsKey($file)) { 
            $dependencyMap.file_purposes[$file] 
        } else { "" }
        
        $fileInfo = @{
            path = $file
            functions = $dependencyMap.definitions[$file]
            count = $functionCount
            purpose = $purpose
        }
        
        # Categorize files
        if ($file -match 'mvgam_core|mvgam\.R|sim_mvgam') {
            $fileCategories['Core'] += $fileInfo
        } elseif ($file -match 'plot') {
            $fileCategories['Plotting'] += $fileInfo
        } elseif ($file -match 'stan|brms|make_stan') {
            $fileCategories['Stan/Modeling'] += $fileInfo
        } elseif ($file -match 'validat|check') {
            $fileCategories['Validation'] += $fileInfo
        } elseif ($file -match 'print|summary|predict|residuals|fitted') {
            $fileCategories['S3 Methods'] += $fileInfo
        } elseif ($file -match 'utils|helpers|globals') {
            $fileCategories['Utilities'] += $fileInfo
        } elseif ($file -match 'data') {
            $fileCategories['Data'] += $fileInfo
        } else {
            $fileCategories['Other'] += $fileInfo
        }
    }
    
    # Output categorized files with rich information
    foreach ($category in $fileCategories.Keys) {
        $files = $fileCategories[$category]
        if ($files.Count -gt 0) {
            $markdown += "`n### $category Files`n"
            $sortedFiles = $files | Sort-Object { $_.count } -Descending
            
            foreach ($fileInfo in $sortedFiles) {
                $markdown += "- **``$($fileInfo.path)``** ($($fileInfo.count) functions)"
                if ($fileInfo.purpose) {
                    $markdown += " - $($fileInfo.purpose)"
                }
                $markdown += "`n"
                
                # Show key functions in this file (prioritize exported, core workflow, and functions with dependencies)
                $exportedInFile = $fileInfo.functions | Where-Object { $_ -in $exportedFunctions }
                $coreInFile = $fileInfo.functions | Where-Object { $_ -in $coreWorkflow }
                $functionsWithDeps = $fileInfo.functions | Where-Object { $dependencyMap.function_dependencies.ContainsKey($_) }
                $otherImportant = $fileInfo.functions | Where-Object { 
                    $_ -match 'validate|check|build|create|make|generate|assemble|inject|merge|stan|brms|prior|trend'
                }
                
                # Combine and prioritize, removing duplicates
                $keyFunctions = @()
                $keyFunctions += $exportedInFile
                $keyFunctions += $coreInFile | Where-Object { $_ -notin $keyFunctions }
                $keyFunctions += $functionsWithDeps | Where-Object { $_ -notin $keyFunctions }
                $keyFunctions += $otherImportant | Where-Object { $_ -notin $keyFunctions }
                
                # Take at least 10, preferably up to 15 functions for important files
                $maxFunctions = if ($fileInfo.path -match $priorityFilesPattern) { 15 } else { 10 }
                $keyFunctions = $keyFunctions | Select-Object -First $maxFunctions
                
                # If we still don't have enough, add remaining functions
                if ($keyFunctions.Count -lt 10) {
                    $remaining = $fileInfo.functions | Where-Object { $_ -notin $keyFunctions } | Select-Object -First (10 - $keyFunctions.Count)
                    $keyFunctions += $remaining
                }
                
                if ($keyFunctions.Count -gt 0) {
                    $markdown += "  - Key functions: " + ($keyFunctions -join ', ')
                    if ($fileInfo.functions.Count -gt $keyFunctions.Count) {
                        $markdown += " (+ $($fileInfo.functions.Count - $keyFunctions.Count) more)"
                    }
                    $markdown += "`n"
                }
            }
        }
    }
    
    # Function location quick reference
    $markdown += "`n## Function Location Quick Reference`n"
    $markdown += "`n### Exported Functions by File`n"
    
    $exportedByFile = @{}
    foreach ($func in $exportedFunctions) {
        if ($dependencyMap.functions.ContainsKey($func)) {
            $file = $dependencyMap.functions[$func]
            if (!$exportedByFile.ContainsKey($file)) {
                $exportedByFile[$file] = @()
            }
            $exportedByFile[$file] += $func
        }
    }
    
    foreach ($file in ($exportedByFile.Keys | Sort-Object)) {
        $functions = $exportedByFile[$file] | Sort-Object
        $markdown += "- **``$file``**: " + ($functions -join ', ') + "`n"
    }
    
    # External package usage analysis for LLM agents
    $markdown += "`n## External Package Integration`n"
    
    # Group external dependencies by likely use case
    $packageUsage = @{
        'Bayesian/MCMC' = @('brms', 'rstan', 'rstantools', 'posterior', 'loo', 'bayesplot')
        'Data Manipulation' = @('dplyr', 'purrr', 'tibble', 'magrittr')
        'Modeling/Statistics' = @('mgcv', 'mvnfast', 'marginaleffects', 'generics')
        'Visualization' = @('ggplot2', 'patchwork')
        'Infrastructure' = @('methods', 'Rcpp', 'rlang', 'insight')
    }
    
    foreach ($category in $packageUsage.Keys) {
        $packages = $packageUsage[$category] | Where-Object { $_ -in $externalDeps }
        if ($packages.Count -gt 0) {
            $markdown += "`n### $category`n"
            foreach ($pkg in $packages) {
                $markdown += "- **$pkg**: "
                # Add common usage descriptions
                switch ($pkg) {
                    'brms' { $markdown += "Bayesian regression model framework integration" }
                    'rstan' { $markdown += "Stan probabilistic programming interface" }
                    'mgcv' { $markdown += "GAM (Generalized Additive Model) functionality" }
                    'ggplot2' { $markdown += "Grammar of graphics plotting system" }
                    'dplyr' { $markdown += "Data frame manipulation and transformation" }
                    'posterior' { $markdown += "Tools for working with posterior distributions" }
                    'loo' { $markdown += "Leave-one-out cross-validation and model comparison" }
                    'marginaleffects' { $markdown += "Marginal effects and predictions" }
                    'insight' { $markdown += "Unified interface to model information" }
                    'Rcpp' { $markdown += "R and C++ integration for performance" }
                    default { $markdown += "Package functionality" }
                }
                $markdown += "`n"
            }
        }
    }
    
    # System-specific function breakdowns
    $markdown += "`n## System-Specific Function Breakdowns`n"
    
    # Prior System Functions
    $priorSystemFuncs = $dependencyMap.functions.Keys | Where-Object { 
        $_ -match 'prior|formula' -and $dependencyMap.functions[$_] -match 'priors|formula'
    } | Sort-Object
    if ($priorSystemFuncs.Count -gt 0) {
        $markdown += "`n### Prior System Functions`n"
        $topPriorFuncs = $priorSystemFuncs | Select-Object -First 8
        foreach ($func in $topPriorFuncs) {
            $file = $dependencyMap.functions[$func]
            $markdown += "- **$func()** (``$file``)"
            if ($dependencyMap.function_dependencies.ContainsKey($func)) {
                $deps = $dependencyMap.function_dependencies[$func]
                $markdown += " - calls: " + (($deps | Select-Object -First 3) -join ', ')
                if ($deps.Count -gt 3) { $markdown += ", ..." }
            }
            $markdown += "`n"
        }
    }
    
    # Trend System Functions
    $trendSystemFuncs = $dependencyMap.functions.Keys | Where-Object { 
        $_ -match 'trend|register.*trend|create.*trend' -and $dependencyMap.functions[$_] -match 'trend_system|mvgam_core'
    } | Sort-Object
    if ($trendSystemFuncs.Count -gt 0) {
        $markdown += "`n### Trend System Functions`n"
        $topTrendFuncs = $trendSystemFuncs | Select-Object -First 8
        foreach ($func in $topTrendFuncs) {
            $file = $dependencyMap.functions[$func]
            $markdown += "- **$func()** (``$file``)"
            if ($dependencyMap.function_dependencies.ContainsKey($func)) {
                $deps = $dependencyMap.function_dependencies[$func]
                $markdown += " - calls: " + (($deps | Select-Object -First 3) -join ', ')
                if ($deps.Count -gt 3) { $markdown += ", ..." }
            }
            $markdown += "`n"
        }
    }
    
    # Stan Assembly System Functions  
    $stanAssemblyFuncs = $dependencyMap.functions.Keys | Where-Object { 
        $_ -match 'stan|assemble|build.*stan|make.*stan|code|data' -and $dependencyMap.functions[$_] -match 'stan_assembly|make_stan|brms_integration'
    } | Sort-Object
    if ($stanAssemblyFuncs.Count -gt 0) {
        $markdown += "`n### Stan Assembly System Functions`n"
        $topStanFuncs = $stanAssemblyFuncs | Select-Object -First 8
        foreach ($func in $topStanFuncs) {
            $file = $dependencyMap.functions[$func]
            $markdown += "- **$func()** (``$file``)"
            if ($dependencyMap.function_dependencies.ContainsKey($func)) {
                $deps = $dependencyMap.function_dependencies[$func]
                $markdown += " - calls: " + (($deps | Select-Object -First 3) -join ', ')
                if ($deps.Count -gt 3) { $markdown += ", ..." }
            }
            $markdown += "`n"
        }
    }
    
    # LLM Agent helpful notes
    $markdown += "`n## Notes for LLM Agents`n"
    $markdown += "- **Primary workflow**: ``mvgam()`` -> prior/trend system setup -> Stan assembly -> model fitting -> S3 methods`n"
    $markdown += "- **Prior system**: ``mvgam_formula()``, ``get_prior()``, ``set_prior()`` for Bayesian prior specification`n"
    $markdown += "- **Trend system**: ``trend_param()``, ``register_trend_type()`` for time series trend modeling`n"
    $markdown += "- **Stan assembly**: ``stancode()``, ``standata()`` for Stan model compilation and data preparation`n"
    $markdown += "- **Stan integration**: Package heavily relies on Stan/brms for Bayesian computation`n" 
    $markdown += "- **S3 system**: Extensive use of S3 classes (``mvgam``, ``mvgam_forecast``, etc.) with method dispatch`n"
    $markdown += "- **File patterns**: ``R/priors.R`` (prior system), ``R/trend_system.R`` (trends), ``R/stan_assembly.R`` (Stan code)`n"
    $markdown += "- **Core functions often call**: validation functions, Stan code generation, data preprocessing utilities`n"
    
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
        exit 0  # Success
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