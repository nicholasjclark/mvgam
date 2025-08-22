# Package-Wide Function Dependency Mapper Hook
# Save as: package-dependency-mapper-hook.ps1
# Triggers: post_git_commit

param([string]$Event, [string]$CommitHash, [string]$CommitMessage)

# Configuration
$CACHE_FILE = ".claude/package-dependency-map.json"
$METADATA_FILE = ".claude/dependency-metadata.json"
$VISUALIZATION_FILE = ".claude/dependency-graph.md"
$BASE_FUNCTIONS = @(
    'c', 'list', 'data.frame', 'length', 'nrow', 'ncol', 'names', 'colnames',
    'mean', 'sum', 'max', 'min', 'paste', 'paste0', 'print', 'cat', 'sprintf',
    'is.null', 'is.na', 'is.numeric', 'is.character', 'as.numeric', 'as.character',
    'apply', 'lapply', 'sapply', 'mapply', 'which', 'match', 'grep', 'gsub'
)

function New-PackageDependencyMap {
    Write-Host "🗺️  Creating package-wide function dependency map..." -ForegroundColor Cyan
    
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
            total_dependencies = 0
        }
        functions = @{}           # function_name -> file_location
        definitions = @{}         # file -> functions_defined
        dependencies = @{}        # function -> functions_it_calls
        reverse_dependencies = @{} # function -> functions_that_call_it
        file_dependencies = @{}   # file -> files_it_depends_on
        external_packages = @{}   # package -> functions_used
        orphaned_functions = @()  # functions with no callers
        circular_dependencies = @() # circular dependency chains
    }
    
    # Step 1: Discover all R files
    $rFiles = Get-AllRFiles
    $dependencyMap.metadata.total_files = $rFiles.Count
    
    Write-Host "📁 Analyzing $($rFiles.Count) R files..." -ForegroundColor Green
    
    # Step 2: Extract all function definitions
    $allFunctions = Get-AllFunctionDefinitions -Files $rFiles
    $dependencyMap.functions = $allFunctions.functions
    $dependencyMap.definitions = $allFunctions.definitions
    $dependencyMap.metadata.total_functions = $allFunctions.functions.Count
    
    # Step 3: Analyze function calls and dependencies
    Write-Host "🔗 Mapping dependencies..." -ForegroundColor Yellow
    $dependencies = Get-FunctionDependencies -Files $rFiles -Functions $allFunctions.functions
    $dependencyMap.dependencies = $dependencies.internal
    $dependencyMap.external_packages = $dependencies.external
    $dependencyMap.metadata.total_dependencies = ($dependencies.internal.Values | ForEach-Object { $_.Count } | Measure-Object -Sum).Sum
    
    # Step 4: Create reverse dependency map
    Write-Host "🔄 Building reverse dependencies..." -ForegroundColor Yellow
    $dependencyMap.reverse_dependencies = Get-ReverseDependencies -Dependencies $dependencies.internal
    
    # Step 5: Calculate file-level dependencies
    Write-Host "📂 Computing file relationships..." -ForegroundColor Yellow
    $dependencyMap.file_dependencies = Get-FileDependencies -FunctionDeps $dependencies.internal -FunctionLocations $allFunctions.functions
    
    # Step 6: Find orphaned functions
    Write-Host "🔍 Analyzing function usage..." -ForegroundColor Yellow
    $dependencyMap.orphaned_functions = Get-OrphanedFunctions -Functions $allFunctions.functions -ReverseDeps $dependencyMap.reverse_dependencies
    
    # Step 7: Detect circular dependencies
    $dependencyMap.circular_dependencies = Get-CircularDependencies -Dependencies $dependencies.internal
    
    # Step 8: Save the dependency map
    Write-Host "💾 Saving results..." -ForegroundColor Yellow
    Save-DependencyMap -Map $dependencyMap
    
    # Step 9: Generate reports
    New-DependencyReports -Map $dependencyMap
    
    $endTime = Get-Date
    $duration = $endTime - $startTime
    Write-Host "✅ Package dependency map completed in $($duration.TotalSeconds.ToString('F1')) seconds" -ForegroundColor Green
    Write-Host "📋 Summary: $($dependencyMap.metadata.total_functions) functions, $($dependencyMap.metadata.total_dependencies) dependencies" -ForegroundColor Cyan
}

function Get-PackageName {
    if (Test-Path "DESCRIPTION") {
        $desc = Get-Content "DESCRIPTION" -Raw
        if ($desc -match "Package:\s*(.+)") {
            return $matches[1].Trim()
        }
    }
    return "unknown"
}

function Get-PackageVersion {
    if (Test-Path "DESCRIPTION") {
        $desc = Get-Content "DESCRIPTION" -Raw
        if ($desc -match "Version:\s*(.+)") {
            return $matches[1].Trim()
        }
    }
    return "unknown"
}

function Get-AllRFiles {
    $rFiles = @()
    
    # Standard R package directories
    $directories = @("R", "tests", "inst", "vignettes")
    
    foreach ($dir in $directories) {
        if (Test-Path $dir) {
            $files = Get-ChildItem -Path $dir -Filter "*.R" -Recurse
            $files += Get-ChildItem -Path $dir -Filter "*.r" -Recurse
            $rFiles += $files | ForEach-Object { $_.FullName }
        }
    }
    
    # Exclude .claude directory
    $rFiles = $rFiles | Where-Object { $_ -notlike "*\.claude\*" }
    
    return $rFiles | Sort-Object
}

function Get-AllFunctionDefinitions {
    param([string[]]$Files)
    
    $functions = @{}     # function_name -> file_path
    $definitions = @{}   # file_path -> [function_names]
    
    foreach ($file in $Files) {
        Write-Host "  📄 Processing $(Split-Path $file -Leaf)..." -ForegroundColor Gray
        
        $content = Get-Content $file -Raw
        $fileFunctions = @()
        
        # Enhanced function definition patterns - covers all R assignment operators
        $patterns = @(
            # Standard assignment with <-
            '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*function\s*\(',  # name <- function(
            
            # Assignment with =
            '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*=\s*function\s*\(',   # name = function(
            
            # Right-hand assignment with ->
            'function\s*\([^)]*\)\s*->\s*([a-zA-Z_][a-zA-Z0-9_.]*)', # function() -> name
            
            # Backticks (for special names)
            '^\s*`([^`]+)`\s*<-\s*function\s*\(',                  # `%>%` <- function(
            '^\s*`([^`]+)`\s*=\s*function\s*\(',                   # `%>%` = function(
            
            # Quoted names (single and double quotes)
            '^\s*"([^"]+)"\s*<-\s*function\s*\(',                  # "name" <- function(
            '^\s*"([^"]+)"\s*=\s*function\s*\(',                   # "name" = function(
            "^\s*'([^']+)'\s*<-\s*function\s*\(",                  # 'name' <- function(
            "^\s*'([^']+)'\s*=\s*function\s*\(",                   # 'name' = function(
            
            # Assignment within lists/environments
            '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*\[\[\s*["\x27]([^"\x27]+)["\x27]\s*\]\]\s*<-\s*function\s*\(', # obj[["name"]] <- function(
            '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*\$\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*function\s*\(',         # obj$name <- function(
            
            # Function factories and closures
            '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*\([^)]*\)\s*\{\s*function\s*\(',  # name <- (args) { function(
            
            # Anonymous functions assigned to variables (common in modern R)
            '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*\\?\s*\([^)]*\)\s*[{-]',           # name <- \(args) or name <- (args) =>
            
            # Base R function assignment patterns
            '^\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*<-\s*function\s*
        
        foreach ($pattern in $patterns) {
            $matches = [regex]::Matches($content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
            foreach ($match in $matches) {
                $functionName = ""
                
                # Extract function name based on pattern type
                if ($pattern -match 'function.*->') {
                    # Right-hand assignment: function() -> name
                    $functionName = $match.Groups[1].Value
                }
                elseif ($pattern -match '\[\[.*\]\]') {
                    # List assignment: obj[["name"]] <- function
                    $functionName = $match.Groups[2].Value  # The quoted name
                }
                elseif ($pattern -match '\
        
        if ($fileFunctions.Count -gt 0) {
            $definitions[$file] = $fileFunctions
            Write-Host "    ✅ Found $($fileFunctions.Count) functions: $($fileFunctions -join ', ')" -ForegroundColor Green
        }
    }
    
    return @{
        functions = $functions
        definitions = $definitions
    }
}

function Get-FunctionDependencies {
    param(
        [string[]]$Files,
        [hashtable]$Functions
    )
    
    $internalDeps = @{}
    $externalDeps = @{}
    
    foreach ($file in $Files) {
        $content = Get-Content $file -Raw
        
        # Get all function calls in this file
        $functionCalls = Get-FunctionCallsFromContent -Content $content
        
        # Get functions defined in this file
        $fileFunctions = $Functions.GetEnumerator() | Where-Object { $_.Value -eq $file } | ForEach-Object { $_.Key }
        
        # For each function in this file, determine what it calls
        foreach ($func in $fileFunctions) {
            # Multiple patterns for function definitions to handle different assignment operators
            $funcPatterns = @(
                "^\s*$func\s*<-\s*function\s*\(",
                "^\s*$func\s*=\s*function\s*\(",
                "function\s*\([^)]*\)\s*->\s*$func",
                "^\s*`$func`\s*<-\s*function\s*\(",
                "^\s*`$func`\s*=\s*function\s*\(",
                "^\s*`"$func"`\s*<-\s*function\s*\(",
                "^\s*`"$func"`\s*=\s*function\s*\(",
                "^\s*'$func'\s*<-\s*function\s*\(",
                "^\s*'$func'\s*=\s*function\s*\("
            )
            
            $funcStart = $null
            foreach ($pattern in $funcPatterns) {
                $funcStart = $content | Select-String -Pattern $pattern
                if ($funcStart) { break }
            }
            
            if ($funcStart) {
                # Extract the function body (simplified - gets content after function definition)
                $lines = $content -split "`n"
                $startLine = $funcStart.LineNumber - 1
                $funcBody = ""
                $braceCount = 0
                $inFunction = $false
                
                for ($i = $startLine; $i -lt $lines.Length; $i++) {
                    $line = $lines[$i]
                    if ($line -match '\{') { 
                        $braceCount += ($line.ToCharArray() | Where-Object { $_ -eq '{' }).Count
                        $inFunction = $true
                    }
                    if ($inFunction) {
                        $funcBody += $line + "`n"
                    }
                    if ($line -match '\}') {
                        $braceCount -= ($line.ToCharArray() | Where-Object { $_ -eq '}' }).Count
                        if ($braceCount -le 0 -and $inFunction) { break }
                    }
                }
                
                # Find function calls within this function body
                $funcCalls = Get-FunctionCallsFromContent -Content $funcBody
                
                # Separate internal vs external calls
                $internalCalls = @()
                foreach ($call in $funcCalls) {
                    if ($call -match '::') {
                        # External package call
                        $parts = $call -split '::'
                        $package = $parts[0]
                        $function = $parts[1]
                        
                        if (-not $externalDeps.ContainsKey($package)) {
                            $externalDeps[$package] = @()
                        }
                        if ($function -notin $externalDeps[$package]) {
                            $externalDeps[$package] += $function
                        }
                    }
                    elseif ($Functions.ContainsKey($call) -and $call -ne $func) {
                        # Internal function call (exclude self-calls)
                        $internalCalls += $call
                    }
                }
                
                if ($internalCalls.Count -gt 0) {
                    $internalDeps[$func] = $internalCalls | Sort-Object -Unique
                }
            }
        }
    }
    
    return @{
        internal = $internalDeps
        external = $externalDeps
    }
}

function Get-FunctionCallsFromContent {
    param([string]$Content)
    
    $functionCalls = @()
    
    # Enhanced patterns for function calls
    $patterns = @(
        '\b([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Standard calls: func()
        '([a-zA-Z_][a-zA-Z0-9_.]+)::\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Package calls: pkg::func()
        '`([^`]+)`\s*\('  # Backtick functions: `%>%`()
    )
    
    foreach ($pattern in $patterns) {
        $matches = [regex]::Matches($Content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
        foreach ($match in $matches) {
            if ($pattern -match '::') {
                # Package::function format
                $functionCalls += "$($match.Groups[1].Value)::$($match.Groups[2].Value)"
            } else {
                # Regular function
                $funcName = $match.Groups[1].Value
                # Exclude base R functions
                if ($funcName -notin $BASE_FUNCTIONS) {
                    $functionCalls += $funcName
                }
            }
        }
    }
    
    return $functionCalls | Sort-Object -Unique
}

function Get-ReverseDependencies {
    param([hashtable]$Dependencies)
    
    $reverseDeps = @{}
    
    # Initialize all functions with empty arrays
    foreach ($func in $Dependencies.Keys) {
        if (-not $reverseDeps.ContainsKey($func)) {
            $reverseDeps[$func] = @()
        }
        
        foreach ($calledFunc in $Dependencies[$func]) {
            if (-not $reverseDeps.ContainsKey($calledFunc)) {
                $reverseDeps[$calledFunc] = @()
            }
            $reverseDeps[$calledFunc] += $func
        }
    }
    
    # Remove duplicates
    foreach ($func in $reverseDeps.Keys) {
        $reverseDeps[$func] = $reverseDeps[$func] | Sort-Object -Unique
    }
    
    return $reverseDeps
}

function Get-FileDependencies {
    param(
        [hashtable]$FunctionDeps,
        [hashtable]$FunctionLocations
    )
    
    $fileDeps = @{}
    
    foreach ($func in $FunctionDeps.Keys) {
        $funcFile = $FunctionLocations[$func]
        if (-not $funcFile) { continue }
        
        if (-not $fileDeps.ContainsKey($funcFile)) {
            $fileDeps[$funcFile] = @()
        }
        
        foreach ($calledFunc in $FunctionDeps[$func]) {
            $calledFile = $FunctionLocations[$calledFunc]
            if ($calledFile -and $calledFile -ne $funcFile) {
                $fileDeps[$funcFile] += $calledFile
            }
        }
        
        # Remove duplicates
        $fileDeps[$funcFile] = $fileDeps[$funcFile] | Sort-Object -Unique
    }
    
    return $fileDeps
}

function Get-OrphanedFunctions {
    param(
        [hashtable]$Functions,
        [hashtable]$ReverseDeps
    )
    
    $orphaned = @()
    
    foreach ($func in $Functions.Keys) {
        if (-not $ReverseDeps.ContainsKey($func) -or $ReverseDeps[$func].Count -eq 0) {
            $orphaned += $func
        }
    }
    
    return $orphaned | Sort-Object
}

function Get-CircularDependencies {
    param([hashtable]$Dependencies)
    
    $circular = @()
    $visited = @{}
    $recursionStack = @{}
    
    function Find-CircularPath {
        param([string]$Function, [string[]]$Path)
        
        if ($recursionStack.ContainsKey($Function)) {
            # Found a cycle - extract the circular part
            $cycleStart = [array]::IndexOf($Path, $Function)
            if ($cycleStart -ge 0) {
                $cycle = $Path[$cycleStart..($Path.Length-1)] + $Function
                return $cycle
            }
        }
        
        if ($visited.ContainsKey($Function)) {
            return $null
        }
        
        $visited[$Function] = $true
        $recursionStack[$Function] = $true
        $newPath = $Path + $Function
        
        if ($Dependencies.ContainsKey($Function)) {
            foreach ($calledFunc in $Dependencies[$Function]) {
                $cycle = Find-CircularPath -Function $calledFunc -Path $newPath
                if ($cycle) {
                    return $cycle
                }
            }
        }
        
        $recursionStack.Remove($Function)
        return $null
    }
    
    foreach ($func in $Dependencies.Keys) {
        if (-not $visited.ContainsKey($func)) {
            $cycle = Find-CircularPath -Function $func -Path @()
            if ($cycle) {
                $circular += ($cycle -join " → ")
            }
        }
    }
    
    return $circular | Sort-Object -Unique
}

function Save-DependencyMap {
    param([hashtable]$Map)
    
    # Ensure .claude directory exists
    if (-not (Test-Path ".claude")) {
        New-Item -ItemType Directory -Path ".claude" -Force | Out-Null
    }
    
    # Save main dependency map
    $Map | ConvertTo-Json -Depth 10 | Out-File -FilePath $CACHE_FILE -Encoding UTF8
    
    # Save metadata separately for quick access
    $Map.metadata | ConvertTo-Json -Depth 3 | Out-File -FilePath $METADATA_FILE -Encoding UTF8
    
    Write-Host "💾 Dependency map saved to $CACHE_FILE" -ForegroundColor Green
    Write-Host "📋 Metadata saved to $METADATA_FILE" -ForegroundColor Green
}

function New-DependencyReports {
    param([hashtable]$Map)
    
    # Generate markdown visualization
    $markdown = @()
    $markdown += "# Package Dependency Map"
    $markdown += ""
    $markdown += "Generated: $($Map.metadata.generated)"
    $markdown += "Package: $($Map.metadata.package_name) v$($Map.metadata.version)"
    $markdown += "Commit: $($Map.metadata.commit_hash)"
    $markdown += ""
    
    # Summary statistics
    $markdown += "## Summary Statistics"
    $markdown += ""
    $markdown += "- **Total Files**: $($Map.metadata.total_files)"
    $markdown += "- **Total Functions**: $($Map.metadata.total_functions)"
    $markdown += "- **Total Dependencies**: $($Map.metadata.total_dependencies)"
    $markdown += "- **Orphaned Functions**: $($Map.orphaned_functions.Count)"
    $markdown += "- **Circular Dependencies**: $($Map.circular_dependencies.Count)"
    $markdown += "- **External Packages**: $($Map.external_packages.Count)"
    $markdown += ""
    
    # File overview
    if ($Map.definitions.Count -gt 0) {
        $markdown += "## File Overview"
        $markdown += ""
        foreach ($file in $Map.definitions.Keys | Sort-Object) {
            $funcs = $Map.definitions[$file]
            $fileSize = if (Test-Path $file) { [math]::Round((Get-Item $file).Length / 1024, 1) } else { "?" }
            $markdown += "### $file ($fileSize KB)"
            $markdown += ""
            $markdown += "Functions: $($funcs -join ', ')"
            $markdown += ""
        }
    }
    
    # External packages
    if ($Map.external_packages.Count -gt 0) {
        $markdown += "## External Package Dependencies"
        $markdown += ""
        foreach ($pkg in $Map.external_packages.Keys | Sort-Object) {
            $funcs = $Map.external_packages[$pkg] -join ', '
            $markdown += "- **$pkg**: $funcs"
        }
        $markdown += ""
    }
    
    # Orphaned functions
    if ($Map.orphaned_functions.Count -gt 0) {
        $markdown += "## Orphaned Functions"
        $markdown += ""
        $markdown += "Functions with no callers (potential candidates for removal):"
        $markdown += ""
        foreach ($func in $Map.orphaned_functions) {
            $file = $Map.functions[$func]
            $markdown += "- **$func** (in $file)"
        }
        $markdown += ""
    }
    
    # Circular dependencies
    if ($Map.circular_dependencies.Count -gt 0) {
        $markdown += "## Circular Dependencies"
        $markdown += ""
        $markdown += "⚠️ These function chains have circular dependencies:"
        $markdown += ""
        foreach ($cycle in $Map.circular_dependencies) {
            $markdown += "- $cycle"
        }
        $markdown += ""
    }
    
    # Save visualization
    $markdown -join "`n" | Out-File -FilePath $VISUALIZATION_FILE -Encoding UTF8
    Write-Host "📊 Visualization saved to $VISUALIZATION_FILE" -ForegroundColor Green
    
    # Quick console summary
    Write-Host ""
    Write-Host "📊 DEPENDENCY MAP SUMMARY" -ForegroundColor Cyan
    Write-Host "=========================" -ForegroundColor Cyan
    Write-Host "📁 Files analyzed: $($Map.metadata.total_files)" -ForegroundColor White
    Write-Host "🔧 Functions found: $($Map.metadata.total_functions)" -ForegroundColor White
    Write-Host "🔗 Dependencies: $($Map.metadata.total_dependencies)" -ForegroundColor White
    
    if ($Map.orphaned_functions.Count -gt 0) {
        Write-Host "🥀 Orphaned functions: $($Map.orphaned_functions.Count)" -ForegroundColor Yellow
        Write-Host "   └─ $($Map.orphaned_functions[0..2] -join ', ')$(if ($Map.orphaned_functions.Count -gt 3) { '...' })" -ForegroundColor Gray
    }
    
    if ($Map.circular_dependencies.Count -gt 0) {
        Write-Host "🔄 Circular dependencies: $($Map.circular_dependencies.Count)" -ForegroundColor Red
    }
    
    if ($Map.external_packages.Count -gt 0) {
        Write-Host "📦 External packages: $($Map.external_packages.Keys -join ', ')" -ForegroundColor Cyan
    }
}

# Main execution
if ($Event -eq "post_git_commit") {
    Write-Host ""
    Write-Host "🚀 PACKAGE DEPENDENCY MAPPER TRIGGERED" -ForegroundColor Green
    Write-Host "=======================================" -ForegroundColor Green
    Write-Host "Commit: $CommitHash" -ForegroundColor White
    Write-Host "Message: $CommitMessage" -ForegroundColor White
    Write-Host ""
    
    New-PackageDependencyMap
    
    Write-Host ""
    Write-Host "🎉 Package dependency mapping complete!" -ForegroundColor Green
    Write-Host "📁 Check .claude/ directory for detailed maps and reports" -ForegroundColor Cyan
    Write-Host ""
}    # name <- function (on separate line)
        )
        
        foreach ($pattern in $patterns) {
            $matches = [regex]::Matches($content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
            foreach ($match in $matches) {
                $functionName = $match.Groups[1].Value
                
                # Skip if already found (avoid duplicates)
                if ($functions.ContainsKey($functionName)) {
                    Write-Host "    ⚠️  Duplicate function '$functionName' found in $file (already in $($functions[$functionName]))" -ForegroundColor Yellow
                    continue
                }
                
                $functions[$functionName] = $file
                $fileFunctions += $functionName
            }
        }
        
        if ($fileFunctions.Count -gt 0) {
            $definitions[$file] = $fileFunctions
            Write-Host "    ✅ Found $($fileFunctions.Count) functions: $($fileFunctions -join ', ')" -ForegroundColor Green
        }
    }
    
    return @{
        functions = $functions
        definitions = $definitions
    }
}

function Get-FunctionDependencies {
    param(
        [string[]]$Files,
        [hashtable]$Functions
    )
    
    $internalDeps = @{}
    $externalDeps = @{}
    
    foreach ($file in $Files) {
        $content = Get-Content $file -Raw
        
        # Get all function calls in this file
        $functionCalls = Get-FunctionCallsFromContent -Content $content
        
        # Get functions defined in this file
        $fileFunctions = $Functions.GetEnumerator() | Where-Object { $_.Value -eq $file } | ForEach-Object { $_.Key }
        
        # For each function in this file, determine what it calls
        foreach ($func in $fileFunctions) {
            $funcPattern = "^\s*$func\s*<-\s*function\s*\("
            $funcStart = $content | Select-String -Pattern $funcPattern
            
            if ($funcStart) {
                # Extract the function body (simplified - gets content after function definition)
                $lines = $content -split "`n"
                $startLine = $funcStart.LineNumber - 1
                $funcBody = ""
                $braceCount = 0
                $inFunction = $false
                
                for ($i = $startLine; $i -lt $lines.Length; $i++) {
                    $line = $lines[$i]
                    if ($line -match '\{') { 
                        $braceCount += ($line.ToCharArray() | Where-Object { $_ -eq '{' }).Count
                        $inFunction = $true
                    }
                    if ($inFunction) {
                        $funcBody += $line + "`n"
                    }
                    if ($line -match '\}') {
                        $braceCount -= ($line.ToCharArray() | Where-Object { $_ -eq '}' }).Count
                        if ($braceCount -le 0 -and $inFunction) { break }
                    }
                }
                
                # Find function calls within this function body
                $funcCalls = Get-FunctionCallsFromContent -Content $funcBody
                
                # Separate internal vs external calls
                $internalCalls = @()
                foreach ($call in $funcCalls) {
                    if ($call -match '::') {
                        # External package call
                        $parts = $call -split '::'
                        $package = $parts[0]
                        $function = $parts[1]
                        
                        if (-not $externalDeps.ContainsKey($package)) {
                            $externalDeps[$package] = @()
                        }
                        if ($function -notin $externalDeps[$package]) {
                            $externalDeps[$package] += $function
                        }
                    }
                    elseif ($Functions.ContainsKey($call) -and $call -ne $func) {
                        # Internal function call (exclude self-calls)
                        $internalCalls += $call
                    }
                }
                
                if ($internalCalls.Count -gt 0) {
                    $internalDeps[$func] = $internalCalls | Sort-Object -Unique
                }
            }
        }
    }
    
    return @{
        internal = $internalDeps
        external = $externalDeps
    }
}

function Get-FunctionCallsFromContent {
    param([string]$Content)
    
    $functionCalls = @()
    
    # Enhanced patterns for function calls
    $patterns = @(
        '\b([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Standard calls: func()
        '([a-zA-Z_][a-zA-Z0-9_.]+)::\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Package calls: pkg::func()
        '`([^`]+)`\s*\('  # Backtick functions: `%>%`()
    )
    
    foreach ($pattern in $patterns) {
        $matches = [regex]::Matches($Content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
        foreach ($match in $matches) {
            if ($pattern -match '::') {
                # Package::function format
                $functionCalls += "$($match.Groups[1].Value)::$($match.Groups[2].Value)"
            } else {
                # Regular function
                $funcName = $match.Groups[1].Value
                # Exclude base R functions
                if ($funcName -notin $BASE_FUNCTIONS) {
                    $functionCalls += $funcName
                }
            }
        }
    }
    
    return $functionCalls | Sort-Object -Unique
}

function Get-ReverseDependencies {
    param([hashtable]$Dependencies)
    
    $reverseDeps = @{}
    
    # Initialize all functions with empty arrays
    foreach ($func in $Dependencies.Keys) {
        if (-not $reverseDeps.ContainsKey($func)) {
            $reverseDeps[$func] = @()
        }
        
        foreach ($calledFunc in $Dependencies[$func]) {
            if (-not $reverseDeps.ContainsKey($calledFunc)) {
                $reverseDeps[$calledFunc] = @()
            }
            $reverseDeps[$calledFunc] += $func
        }
    }
    
    # Remove duplicates
    foreach ($func in $reverseDeps.Keys) {
        $reverseDeps[$func] = $reverseDeps[$func] | Sort-Object -Unique
    }
    
    return $reverseDeps
}

function Get-FileDependencies {
    param(
        [hashtable]$FunctionDeps,
        [hashtable]$FunctionLocations
    )
    
    $fileDeps = @{}
    
    foreach ($func in $FunctionDeps.Keys) {
        $funcFile = $FunctionLocations[$func]
        if (-not $funcFile) { continue }
        
        if (-not $fileDeps.ContainsKey($funcFile)) {
            $fileDeps[$funcFile] = @()
        }
        
        foreach ($calledFunc in $FunctionDeps[$func]) {
            $calledFile = $FunctionLocations[$calledFunc]
            if ($calledFile -and $calledFile -ne $funcFile) {
                $fileDeps[$funcFile] += $calledFile
            }
        }
        
        # Remove duplicates
        $fileDeps[$funcFile] = $fileDeps[$funcFile] | Sort-Object -Unique
    }
    
    return $fileDeps
}

function Get-OrphanedFunctions {
    param(
        [hashtable]$Functions,
        [hashtable]$ReverseDeps
    )
    
    $orphaned = @()
    
    foreach ($func in $Functions.Keys) {
        if (-not $ReverseDeps.ContainsKey($func) -or $ReverseDeps[$func].Count -eq 0) {
            $orphaned += $func
        }
    }
    
    return $orphaned | Sort-Object
}

function Get-CircularDependencies {
    param([hashtable]$Dependencies)
    
    $circular = @()
    $visited = @{}
    $recursionStack = @{}
    
    function Find-CircularPath {
        param([string]$Function, [string[]]$Path)
        
        if ($recursionStack.ContainsKey($Function)) {
            # Found a cycle - extract the circular part
            $cycleStart = [array]::IndexOf($Path, $Function)
            if ($cycleStart -ge 0) {
                $cycle = $Path[$cycleStart..($Path.Length-1)] + $Function
                return $cycle
            }
        }
        
        if ($visited.ContainsKey($Function)) {
            return $null
        }
        
        $visited[$Function] = $true
        $recursionStack[$Function] = $true
        $newPath = $Path + $Function
        
        if ($Dependencies.ContainsKey($Function)) {
            foreach ($calledFunc in $Dependencies[$Function]) {
                $cycle = Find-CircularPath -Function $calledFunc -Path $newPath
                if ($cycle) {
                    return $cycle
                }
            }
        }
        
        $recursionStack.Remove($Function)
        return $null
    }
    
    foreach ($func in $Dependencies.Keys) {
        if (-not $visited.ContainsKey($func)) {
            $cycle = Find-CircularPath -Function $func -Path @()
            if ($cycle) {
                $circular += ($cycle -join " → ")
            }
        }
    }
    
    return $circular | Sort-Object -Unique
}

function Save-DependencyMap {
    param([hashtable]$Map)
    
    # Ensure .claude directory exists
    if (-not (Test-Path ".claude")) {
        New-Item -ItemType Directory -Path ".claude" -Force | Out-Null
    }
    
    # Save main dependency map
    $Map | ConvertTo-Json -Depth 10 | Out-File -FilePath $CACHE_FILE -Encoding UTF8
    
    # Save metadata separately for quick access
    $Map.metadata | ConvertTo-Json -Depth 3 | Out-File -FilePath $METADATA_FILE -Encoding UTF8
    
    Write-Host "💾 Dependency map saved to $CACHE_FILE" -ForegroundColor Green
    Write-Host "📋 Metadata saved to $METADATA_FILE" -ForegroundColor Green
}

function New-DependencyReports {
    param([hashtable]$Map)
    
    # Generate markdown visualization
    $markdown = @()
    $markdown += "# Package Dependency Map"
    $markdown += ""
    $markdown += "Generated: $($Map.metadata.generated)"
    $markdown += "Package: $($Map.metadata.package_name) v$($Map.metadata.version)"
    $markdown += "Commit: $($Map.metadata.commit_hash)"
    $markdown += ""
    
    # Summary statistics
    $markdown += "## Summary Statistics"
    $markdown += ""
    $markdown += "- **Total Files**: $($Map.metadata.total_files)"
    $markdown += "- **Total Functions**: $($Map.metadata.total_functions)"
    $markdown += "- **Total Dependencies**: $($Map.metadata.total_dependencies)"
    $markdown += "- **Orphaned Functions**: $($Map.orphaned_functions.Count)"
    $markdown += "- **Circular Dependencies**: $($Map.circular_dependencies.Count)"
    $markdown += "- **External Packages**: $($Map.external_packages.Count)"
    $markdown += ""
    
    # File overview
    if ($Map.definitions.Count -gt 0) {
        $markdown += "## File Overview"
        $markdown += ""
        foreach ($file in $Map.definitions.Keys | Sort-Object) {
            $funcs = $Map.definitions[$file]
            $fileSize = if (Test-Path $file) { [math]::Round((Get-Item $file).Length / 1024, 1) } else { "?" }
            $markdown += "### $file ($fileSize KB)"
            $markdown += ""
            $markdown += "Functions: $($funcs -join ', ')"
            $markdown += ""
        }
    }
    
    # External packages
    if ($Map.external_packages.Count -gt 0) {
        $markdown += "## External Package Dependencies"
        $markdown += ""
        foreach ($pkg in $Map.external_packages.Keys | Sort-Object) {
            $funcs = $Map.external_packages[$pkg] -join ', '
            $markdown += "- **$pkg**: $funcs"
        }
        $markdown += ""
    }
    
    # Orphaned functions
    if ($Map.orphaned_functions.Count -gt 0) {
        $markdown += "## Orphaned Functions"
        $markdown += ""
        $markdown += "Functions with no callers (potential candidates for removal):"
        $markdown += ""
        foreach ($func in $Map.orphaned_functions) {
            $file = $Map.functions[$func]
            $markdown += "- **$func** (in $file)"
        }
        $markdown += ""
    }
    
    # Circular dependencies
    if ($Map.circular_dependencies.Count -gt 0) {
        $markdown += "## Circular Dependencies"
        $markdown += ""
        $markdown += "⚠️ These function chains have circular dependencies:"
        $markdown += ""
        foreach ($cycle in $Map.circular_dependencies) {
            $markdown += "- $cycle"
        }
        $markdown += ""
    }
    
    # Save visualization
    $markdown -join "`n" | Out-File -FilePath $VISUALIZATION_FILE -Encoding UTF8
    Write-Host "📊 Visualization saved to $VISUALIZATION_FILE" -ForegroundColor Green
    
    # Quick console summary
    Write-Host ""
    Write-Host "📊 DEPENDENCY MAP SUMMARY" -ForegroundColor Cyan
    Write-Host "=========================" -ForegroundColor Cyan
    Write-Host "📁 Files analyzed: $($Map.metadata.total_files)" -ForegroundColor White
    Write-Host "🔧 Functions found: $($Map.metadata.total_functions)" -ForegroundColor White
    Write-Host "🔗 Dependencies: $($Map.metadata.total_dependencies)" -ForegroundColor White
    
    if ($Map.orphaned_functions.Count -gt 0) {
        Write-Host "🥀 Orphaned functions: $($Map.orphaned_functions.Count)" -ForegroundColor Yellow
        Write-Host "   └─ $($Map.orphaned_functions[0..2] -join ', ')$(if ($Map.orphaned_functions.Count -gt 3) { '...' })" -ForegroundColor Gray
    }
    
    if ($Map.circular_dependencies.Count -gt 0) {
        Write-Host "🔄 Circular dependencies: $($Map.circular_dependencies.Count)" -ForegroundColor Red
    }
    
    if ($Map.external_packages.Count -gt 0) {
        Write-Host "📦 External packages: $($Map.external_packages.Keys -join ', ')" -ForegroundColor Cyan
    }
}

# Main execution
if ($Event -eq "post_git_commit") {
    Write-Host ""
    Write-Host "🚀 PACKAGE DEPENDENCY MAPPER TRIGGERED" -ForegroundColor Green
    Write-Host "=======================================" -ForegroundColor Green
    Write-Host "Commit: $CommitHash" -ForegroundColor White
    Write-Host "Message: $CommitMessage" -ForegroundColor White
    Write-Host ""
    
    New-PackageDependencyMap
    
    Write-Host ""
    Write-Host "🎉 Package dependency mapping complete!" -ForegroundColor Green
    Write-Host "📁 Check .claude/ directory for detailed maps and reports" -ForegroundColor Cyan
    Write-Host ""
}) {
                    # Object property: obj$name <- function
                    $functionName = $match.Groups[2].Value  # The property name
                }
                elseif ($pattern -match '\\?\s*\(.*\)\s*[{-]') {
                    # Anonymous function: name <- \(args) or name <- (args) =>
                    $functionName = $match.Groups[1].Value
                }
                else {
                    # Standard patterns: name <- function, name = function, etc.
                    $functionName = $match.Groups[1].Value
                }
                
                # Clean up function name
                $functionName = $functionName.Trim()
                
                # Skip empty names
                if (-not $functionName) { continue }
                
                # Skip if already found (avoid duplicates)
                if ($functions.ContainsKey($functionName)) {
                    Write-Host "    ⚠️  Duplicate function '$functionName' found in $file (already in $($functions[$functionName]))" -ForegroundColor Yellow
                    continue
                }
                
                $functions[$functionName] = $file
                $fileFunctions += $functionName
                
                # Log the pattern that matched for debugging
                $patternType = switch -Regex ($pattern) {
                    '<-.*function' { "standard assignment" }
                    '=.*function' { "equals assignment" }
                    'function.*->' { "right-hand assignment" }
                    '`.*`' { "backtick name" }
                    '".*"' { "quoted name" }
                    "'" { "single-quoted name" }
                    '\[\[' { "list element assignment" }
                    '\
        
        if ($fileFunctions.Count -gt 0) {
            $definitions[$file] = $fileFunctions
            Write-Host "    ✅ Found $($fileFunctions.Count) functions: $($fileFunctions -join ', ')" -ForegroundColor Green
        }
    }
    
    return @{
        functions = $functions
        definitions = $definitions
    }
}

function Get-FunctionDependencies {
    param(
        [string[]]$Files,
        [hashtable]$Functions
    )
    
    $internalDeps = @{}
    $externalDeps = @{}
    
    foreach ($file in $Files) {
        $content = Get-Content $file -Raw
        
        # Get all function calls in this file
        $functionCalls = Get-FunctionCallsFromContent -Content $content
        
        # Get functions defined in this file
        $fileFunctions = $Functions.GetEnumerator() | Where-Object { $_.Value -eq $file } | ForEach-Object { $_.Key }
        
        # For each function in this file, determine what it calls
        foreach ($func in $fileFunctions) {
            $funcPattern = "^\s*$func\s*<-\s*function\s*\("
            $funcStart = $content | Select-String -Pattern $funcPattern
            
            if ($funcStart) {
                # Extract the function body (simplified - gets content after function definition)
                $lines = $content -split "`n"
                $startLine = $funcStart.LineNumber - 1
                $funcBody = ""
                $braceCount = 0
                $inFunction = $false
                
                for ($i = $startLine; $i -lt $lines.Length; $i++) {
                    $line = $lines[$i]
                    if ($line -match '\{') { 
                        $braceCount += ($line.ToCharArray() | Where-Object { $_ -eq '{' }).Count
                        $inFunction = $true
                    }
                    if ($inFunction) {
                        $funcBody += $line + "`n"
                    }
                    if ($line -match '\}') {
                        $braceCount -= ($line.ToCharArray() | Where-Object { $_ -eq '}' }).Count
                        if ($braceCount -le 0 -and $inFunction) { break }
                    }
                }
                
                # Find function calls within this function body
                $funcCalls = Get-FunctionCallsFromContent -Content $funcBody
                
                # Separate internal vs external calls
                $internalCalls = @()
                foreach ($call in $funcCalls) {
                    if ($call -match '::') {
                        # External package call
                        $parts = $call -split '::'
                        $package = $parts[0]
                        $function = $parts[1]
                        
                        if (-not $externalDeps.ContainsKey($package)) {
                            $externalDeps[$package] = @()
                        }
                        if ($function -notin $externalDeps[$package]) {
                            $externalDeps[$package] += $function
                        }
                    }
                    elseif ($Functions.ContainsKey($call) -and $call -ne $func) {
                        # Internal function call (exclude self-calls)
                        $internalCalls += $call
                    }
                }
                
                if ($internalCalls.Count -gt 0) {
                    $internalDeps[$func] = $internalCalls | Sort-Object -Unique
                }
            }
        }
    }
    
    return @{
        internal = $internalDeps
        external = $externalDeps
    }
}

function Get-FunctionCallsFromContent {
    param([string]$Content)
    
    $functionCalls = @()
    
    # Enhanced patterns for function calls
    $patterns = @(
        '\b([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Standard calls: func()
        '([a-zA-Z_][a-zA-Z0-9_.]+)::\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Package calls: pkg::func()
        '`([^`]+)`\s*\('  # Backtick functions: `%>%`()
    )
    
    foreach ($pattern in $patterns) {
        $matches = [regex]::Matches($Content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
        foreach ($match in $matches) {
            if ($pattern -match '::') {
                # Package::function format
                $functionCalls += "$($match.Groups[1].Value)::$($match.Groups[2].Value)"
            } else {
                # Regular function
                $funcName = $match.Groups[1].Value
                # Exclude base R functions
                if ($funcName -notin $BASE_FUNCTIONS) {
                    $functionCalls += $funcName
                }
            }
        }
    }
    
    return $functionCalls | Sort-Object -Unique
}

function Get-ReverseDependencies {
    param([hashtable]$Dependencies)
    
    $reverseDeps = @{}
    
    # Initialize all functions with empty arrays
    foreach ($func in $Dependencies.Keys) {
        if (-not $reverseDeps.ContainsKey($func)) {
            $reverseDeps[$func] = @()
        }
        
        foreach ($calledFunc in $Dependencies[$func]) {
            if (-not $reverseDeps.ContainsKey($calledFunc)) {
                $reverseDeps[$calledFunc] = @()
            }
            $reverseDeps[$calledFunc] += $func
        }
    }
    
    # Remove duplicates
    foreach ($func in $reverseDeps.Keys) {
        $reverseDeps[$func] = $reverseDeps[$func] | Sort-Object -Unique
    }
    
    return $reverseDeps
}

function Get-FileDependencies {
    param(
        [hashtable]$FunctionDeps,
        [hashtable]$FunctionLocations
    )
    
    $fileDeps = @{}
    
    foreach ($func in $FunctionDeps.Keys) {
        $funcFile = $FunctionLocations[$func]
        if (-not $funcFile) { continue }
        
        if (-not $fileDeps.ContainsKey($funcFile)) {
            $fileDeps[$funcFile] = @()
        }
        
        foreach ($calledFunc in $FunctionDeps[$func]) {
            $calledFile = $FunctionLocations[$calledFunc]
            if ($calledFile -and $calledFile -ne $funcFile) {
                $fileDeps[$funcFile] += $calledFile
            }
        }
        
        # Remove duplicates
        $fileDeps[$funcFile] = $fileDeps[$funcFile] | Sort-Object -Unique
    }
    
    return $fileDeps
}

function Get-OrphanedFunctions {
    param(
        [hashtable]$Functions,
        [hashtable]$ReverseDeps
    )
    
    $orphaned = @()
    
    foreach ($func in $Functions.Keys) {
        if (-not $ReverseDeps.ContainsKey($func) -or $ReverseDeps[$func].Count -eq 0) {
            $orphaned += $func
        }
    }
    
    return $orphaned | Sort-Object
}

function Get-CircularDependencies {
    param([hashtable]$Dependencies)
    
    $circular = @()
    $visited = @{}
    $recursionStack = @{}
    
    function Find-CircularPath {
        param([string]$Function, [string[]]$Path)
        
        if ($recursionStack.ContainsKey($Function)) {
            # Found a cycle - extract the circular part
            $cycleStart = [array]::IndexOf($Path, $Function)
            if ($cycleStart -ge 0) {
                $cycle = $Path[$cycleStart..($Path.Length-1)] + $Function
                return $cycle
            }
        }
        
        if ($visited.ContainsKey($Function)) {
            return $null
        }
        
        $visited[$Function] = $true
        $recursionStack[$Function] = $true
        $newPath = $Path + $Function
        
        if ($Dependencies.ContainsKey($Function)) {
            foreach ($calledFunc in $Dependencies[$Function]) {
                $cycle = Find-CircularPath -Function $calledFunc -Path $newPath
                if ($cycle) {
                    return $cycle
                }
            }
        }
        
        $recursionStack.Remove($Function)
        return $null
    }
    
    foreach ($func in $Dependencies.Keys) {
        if (-not $visited.ContainsKey($func)) {
            $cycle = Find-CircularPath -Function $func -Path @()
            if ($cycle) {
                $circular += ($cycle -join " → ")
            }
        }
    }
    
    return $circular | Sort-Object -Unique
}

function Save-DependencyMap {
    param([hashtable]$Map)
    
    # Ensure .claude directory exists
    if (-not (Test-Path ".claude")) {
        New-Item -ItemType Directory -Path ".claude" -Force | Out-Null
    }
    
    # Save main dependency map
    $Map | ConvertTo-Json -Depth 10 | Out-File -FilePath $CACHE_FILE -Encoding UTF8
    
    # Save metadata separately for quick access
    $Map.metadata | ConvertTo-Json -Depth 3 | Out-File -FilePath $METADATA_FILE -Encoding UTF8
    
    Write-Host "💾 Dependency map saved to $CACHE_FILE" -ForegroundColor Green
    Write-Host "📋 Metadata saved to $METADATA_FILE" -ForegroundColor Green
}

function New-DependencyReports {
    param([hashtable]$Map)
    
    # Generate markdown visualization
    $markdown = @()
    $markdown += "# Package Dependency Map"
    $markdown += ""
    $markdown += "Generated: $($Map.metadata.generated)"
    $markdown += "Package: $($Map.metadata.package_name) v$($Map.metadata.version)"
    $markdown += "Commit: $($Map.metadata.commit_hash)"
    $markdown += ""
    
    # Summary statistics
    $markdown += "## Summary Statistics"
    $markdown += ""
    $markdown += "- **Total Files**: $($Map.metadata.total_files)"
    $markdown += "- **Total Functions**: $($Map.metadata.total_functions)"
    $markdown += "- **Total Dependencies**: $($Map.metadata.total_dependencies)"
    $markdown += "- **Orphaned Functions**: $($Map.orphaned_functions.Count)"
    $markdown += "- **Circular Dependencies**: $($Map.circular_dependencies.Count)"
    $markdown += "- **External Packages**: $($Map.external_packages.Count)"
    $markdown += ""
    
    # File overview
    if ($Map.definitions.Count -gt 0) {
        $markdown += "## File Overview"
        $markdown += ""
        foreach ($file in $Map.definitions.Keys | Sort-Object) {
            $funcs = $Map.definitions[$file]
            $fileSize = if (Test-Path $file) { [math]::Round((Get-Item $file).Length / 1024, 1) } else { "?" }
            $markdown += "### $file ($fileSize KB)"
            $markdown += ""
            $markdown += "Functions: $($funcs -join ', ')"
            $markdown += ""
        }
    }
    
    # External packages
    if ($Map.external_packages.Count -gt 0) {
        $markdown += "## External Package Dependencies"
        $markdown += ""
        foreach ($pkg in $Map.external_packages.Keys | Sort-Object) {
            $funcs = $Map.external_packages[$pkg] -join ', '
            $markdown += "- **$pkg**: $funcs"
        }
        $markdown += ""
    }
    
    # Orphaned functions
    if ($Map.orphaned_functions.Count -gt 0) {
        $markdown += "## Orphaned Functions"
        $markdown += ""
        $markdown += "Functions with no callers (potential candidates for removal):"
        $markdown += ""
        foreach ($func in $Map.orphaned_functions) {
            $file = $Map.functions[$func]
            $markdown += "- **$func** (in $file)"
        }
        $markdown += ""
    }
    
    # Circular dependencies
    if ($Map.circular_dependencies.Count -gt 0) {
        $markdown += "## Circular Dependencies"
        $markdown += ""
        $markdown += "⚠️ These function chains have circular dependencies:"
        $markdown += ""
        foreach ($cycle in $Map.circular_dependencies) {
            $markdown += "- $cycle"
        }
        $markdown += ""
    }
    
    # Save visualization
    $markdown -join "`n" | Out-File -FilePath $VISUALIZATION_FILE -Encoding UTF8
    Write-Host "📊 Visualization saved to $VISUALIZATION_FILE" -ForegroundColor Green
    
    # Quick console summary
    Write-Host ""
    Write-Host "📊 DEPENDENCY MAP SUMMARY" -ForegroundColor Cyan
    Write-Host "=========================" -ForegroundColor Cyan
    Write-Host "📁 Files analyzed: $($Map.metadata.total_files)" -ForegroundColor White
    Write-Host "🔧 Functions found: $($Map.metadata.total_functions)" -ForegroundColor White
    Write-Host "🔗 Dependencies: $($Map.metadata.total_dependencies)" -ForegroundColor White
    
    if ($Map.orphaned_functions.Count -gt 0) {
        Write-Host "🥀 Orphaned functions: $($Map.orphaned_functions.Count)" -ForegroundColor Yellow
        Write-Host "   └─ $($Map.orphaned_functions[0..2] -join ', ')$(if ($Map.orphaned_functions.Count -gt 3) { '...' })" -ForegroundColor Gray
    }
    
    if ($Map.circular_dependencies.Count -gt 0) {
        Write-Host "🔄 Circular dependencies: $($Map.circular_dependencies.Count)" -ForegroundColor Red
    }
    
    if ($Map.external_packages.Count -gt 0) {
        Write-Host "📦 External packages: $($Map.external_packages.Keys -join ', ')" -ForegroundColor Cyan
    }
}

# Main execution
if ($Event -eq "post_git_commit") {
    Write-Host ""
    Write-Host "🚀 PACKAGE DEPENDENCY MAPPER TRIGGERED" -ForegroundColor Green
    Write-Host "=======================================" -ForegroundColor Green
    Write-Host "Commit: $CommitHash" -ForegroundColor White
    Write-Host "Message: $CommitMessage" -ForegroundColor White
    Write-Host ""
    
    New-PackageDependencyMap
    
    Write-Host ""
    Write-Host "🎉 Package dependency mapping complete!" -ForegroundColor Green
    Write-Host "📁 Check .claude/ directory for detailed maps and reports" -ForegroundColor Cyan
    Write-Host ""
}    # name <- function (on separate line)
        )
        
        foreach ($pattern in $patterns) {
            $matches = [regex]::Matches($content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
            foreach ($match in $matches) {
                $functionName = $match.Groups[1].Value
                
                # Skip if already found (avoid duplicates)
                if ($functions.ContainsKey($functionName)) {
                    Write-Host "    ⚠️  Duplicate function '$functionName' found in $file (already in $($functions[$functionName]))" -ForegroundColor Yellow
                    continue
                }
                
                $functions[$functionName] = $file
                $fileFunctions += $functionName
            }
        }
        
        if ($fileFunctions.Count -gt 0) {
            $definitions[$file] = $fileFunctions
            Write-Host "    ✅ Found $($fileFunctions.Count) functions: $($fileFunctions -join ', ')" -ForegroundColor Green
        }
    }
    
    return @{
        functions = $functions
        definitions = $definitions
    }
}

function Get-FunctionDependencies {
    param(
        [string[]]$Files,
        [hashtable]$Functions
    )
    
    $internalDeps = @{}
    $externalDeps = @{}
    
    foreach ($file in $Files) {
        $content = Get-Content $file -Raw
        
        # Get all function calls in this file
        $functionCalls = Get-FunctionCallsFromContent -Content $content
        
        # Get functions defined in this file
        $fileFunctions = $Functions.GetEnumerator() | Where-Object { $_.Value -eq $file } | ForEach-Object { $_.Key }
        
        # For each function in this file, determine what it calls
        foreach ($func in $fileFunctions) {
            $funcPattern = "^\s*$func\s*<-\s*function\s*\("
            $funcStart = $content | Select-String -Pattern $funcPattern
            
            if ($funcStart) {
                # Extract the function body (simplified - gets content after function definition)
                $lines = $content -split "`n"
                $startLine = $funcStart.LineNumber - 1
                $funcBody = ""
                $braceCount = 0
                $inFunction = $false
                
                for ($i = $startLine; $i -lt $lines.Length; $i++) {
                    $line = $lines[$i]
                    if ($line -match '\{') { 
                        $braceCount += ($line.ToCharArray() | Where-Object { $_ -eq '{' }).Count
                        $inFunction = $true
                    }
                    if ($inFunction) {
                        $funcBody += $line + "`n"
                    }
                    if ($line -match '\}') {
                        $braceCount -= ($line.ToCharArray() | Where-Object { $_ -eq '}' }).Count
                        if ($braceCount -le 0 -and $inFunction) { break }
                    }
                }
                
                # Find function calls within this function body
                $funcCalls = Get-FunctionCallsFromContent -Content $funcBody
                
                # Separate internal vs external calls
                $internalCalls = @()
                foreach ($call in $funcCalls) {
                    if ($call -match '::') {
                        # External package call
                        $parts = $call -split '::'
                        $package = $parts[0]
                        $function = $parts[1]
                        
                        if (-not $externalDeps.ContainsKey($package)) {
                            $externalDeps[$package] = @()
                        }
                        if ($function -notin $externalDeps[$package]) {
                            $externalDeps[$package] += $function
                        }
                    }
                    elseif ($Functions.ContainsKey($call) -and $call -ne $func) {
                        # Internal function call (exclude self-calls)
                        $internalCalls += $call
                    }
                }
                
                if ($internalCalls.Count -gt 0) {
                    $internalDeps[$func] = $internalCalls | Sort-Object -Unique
                }
            }
        }
    }
    
    return @{
        internal = $internalDeps
        external = $externalDeps
    }
}

function Get-FunctionCallsFromContent {
    param([string]$Content)
    
    $functionCalls = @()
    
    # Enhanced patterns for function calls
    $patterns = @(
        '\b([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Standard calls: func()
        '([a-zA-Z_][a-zA-Z0-9_.]+)::\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Package calls: pkg::func()
        '`([^`]+)`\s*\('  # Backtick functions: `%>%`()
    )
    
    foreach ($pattern in $patterns) {
        $matches = [regex]::Matches($Content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
        foreach ($match in $matches) {
            if ($pattern -match '::') {
                # Package::function format
                $functionCalls += "$($match.Groups[1].Value)::$($match.Groups[2].Value)"
            } else {
                # Regular function
                $funcName = $match.Groups[1].Value
                # Exclude base R functions
                if ($funcName -notin $BASE_FUNCTIONS) {
                    $functionCalls += $funcName
                }
            }
        }
    }
    
    return $functionCalls | Sort-Object -Unique
}

function Get-ReverseDependencies {
    param([hashtable]$Dependencies)
    
    $reverseDeps = @{}
    
    # Initialize all functions with empty arrays
    foreach ($func in $Dependencies.Keys) {
        if (-not $reverseDeps.ContainsKey($func)) {
            $reverseDeps[$func] = @()
        }
        
        foreach ($calledFunc in $Dependencies[$func]) {
            if (-not $reverseDeps.ContainsKey($calledFunc)) {
                $reverseDeps[$calledFunc] = @()
            }
            $reverseDeps[$calledFunc] += $func
        }
    }
    
    # Remove duplicates
    foreach ($func in $reverseDeps.Keys) {
        $reverseDeps[$func] = $reverseDeps[$func] | Sort-Object -Unique
    }
    
    return $reverseDeps
}

function Get-FileDependencies {
    param(
        [hashtable]$FunctionDeps,
        [hashtable]$FunctionLocations
    )
    
    $fileDeps = @{}
    
    foreach ($func in $FunctionDeps.Keys) {
        $funcFile = $FunctionLocations[$func]
        if (-not $funcFile) { continue }
        
        if (-not $fileDeps.ContainsKey($funcFile)) {
            $fileDeps[$funcFile] = @()
        }
        
        foreach ($calledFunc in $FunctionDeps[$func]) {
            $calledFile = $FunctionLocations[$calledFunc]
            if ($calledFile -and $calledFile -ne $funcFile) {
                $fileDeps[$funcFile] += $calledFile
            }
        }
        
        # Remove duplicates
        $fileDeps[$funcFile] = $fileDeps[$funcFile] | Sort-Object -Unique
    }
    
    return $fileDeps
}

function Get-OrphanedFunctions {
    param(
        [hashtable]$Functions,
        [hashtable]$ReverseDeps
    )
    
    $orphaned = @()
    
    foreach ($func in $Functions.Keys) {
        if (-not $ReverseDeps.ContainsKey($func) -or $ReverseDeps[$func].Count -eq 0) {
            $orphaned += $func
        }
    }
    
    return $orphaned | Sort-Object
}

function Get-CircularDependencies {
    param([hashtable]$Dependencies)
    
    $circular = @()
    $visited = @{}
    $recursionStack = @{}
    
    function Find-CircularPath {
        param([string]$Function, [string[]]$Path)
        
        if ($recursionStack.ContainsKey($Function)) {
            # Found a cycle - extract the circular part
            $cycleStart = [array]::IndexOf($Path, $Function)
            if ($cycleStart -ge 0) {
                $cycle = $Path[$cycleStart..($Path.Length-1)] + $Function
                return $cycle
            }
        }
        
        if ($visited.ContainsKey($Function)) {
            return $null
        }
        
        $visited[$Function] = $true
        $recursionStack[$Function] = $true
        $newPath = $Path + $Function
        
        if ($Dependencies.ContainsKey($Function)) {
            foreach ($calledFunc in $Dependencies[$Function]) {
                $cycle = Find-CircularPath -Function $calledFunc -Path $newPath
                if ($cycle) {
                    return $cycle
                }
            }
        }
        
        $recursionStack.Remove($Function)
        return $null
    }
    
    foreach ($func in $Dependencies.Keys) {
        if (-not $visited.ContainsKey($func)) {
            $cycle = Find-CircularPath -Function $func -Path @()
            if ($cycle) {
                $circular += ($cycle -join " → ")
            }
        }
    }
    
    return $circular | Sort-Object -Unique
}

function Save-DependencyMap {
    param([hashtable]$Map)
    
    # Ensure .claude directory exists
    if (-not (Test-Path ".claude")) {
        New-Item -ItemType Directory -Path ".claude" -Force | Out-Null
    }
    
    # Save main dependency map
    $Map | ConvertTo-Json -Depth 10 | Out-File -FilePath $CACHE_FILE -Encoding UTF8
    
    # Save metadata separately for quick access
    $Map.metadata | ConvertTo-Json -Depth 3 | Out-File -FilePath $METADATA_FILE -Encoding UTF8
    
    Write-Host "💾 Dependency map saved to $CACHE_FILE" -ForegroundColor Green
    Write-Host "📋 Metadata saved to $METADATA_FILE" -ForegroundColor Green
}

function New-DependencyReports {
    param([hashtable]$Map)
    
    # Generate markdown visualization
    $markdown = @()
    $markdown += "# Package Dependency Map"
    $markdown += ""
    $markdown += "Generated: $($Map.metadata.generated)"
    $markdown += "Package: $($Map.metadata.package_name) v$($Map.metadata.version)"
    $markdown += "Commit: $($Map.metadata.commit_hash)"
    $markdown += ""
    
    # Summary statistics
    $markdown += "## Summary Statistics"
    $markdown += ""
    $markdown += "- **Total Files**: $($Map.metadata.total_files)"
    $markdown += "- **Total Functions**: $($Map.metadata.total_functions)"
    $markdown += "- **Total Dependencies**: $($Map.metadata.total_dependencies)"
    $markdown += "- **Orphaned Functions**: $($Map.orphaned_functions.Count)"
    $markdown += "- **Circular Dependencies**: $($Map.circular_dependencies.Count)"
    $markdown += "- **External Packages**: $($Map.external_packages.Count)"
    $markdown += ""
    
    # File overview
    if ($Map.definitions.Count -gt 0) {
        $markdown += "## File Overview"
        $markdown += ""
        foreach ($file in $Map.definitions.Keys | Sort-Object) {
            $funcs = $Map.definitions[$file]
            $fileSize = if (Test-Path $file) { [math]::Round((Get-Item $file).Length / 1024, 1) } else { "?" }
            $markdown += "### $file ($fileSize KB)"
            $markdown += ""
            $markdown += "Functions: $($funcs -join ', ')"
            $markdown += ""
        }
    }
    
    # External packages
    if ($Map.external_packages.Count -gt 0) {
        $markdown += "## External Package Dependencies"
        $markdown += ""
        foreach ($pkg in $Map.external_packages.Keys | Sort-Object) {
            $funcs = $Map.external_packages[$pkg] -join ', '
            $markdown += "- **$pkg**: $funcs"
        }
        $markdown += ""
    }
    
    # Orphaned functions
    if ($Map.orphaned_functions.Count -gt 0) {
        $markdown += "## Orphaned Functions"
        $markdown += ""
        $markdown += "Functions with no callers (potential candidates for removal):"
        $markdown += ""
        foreach ($func in $Map.orphaned_functions) {
            $file = $Map.functions[$func]
            $markdown += "- **$func** (in $file)"
        }
        $markdown += ""
    }
    
    # Circular dependencies
    if ($Map.circular_dependencies.Count -gt 0) {
        $markdown += "## Circular Dependencies"
        $markdown += ""
        $markdown += "⚠️ These function chains have circular dependencies:"
        $markdown += ""
        foreach ($cycle in $Map.circular_dependencies) {
            $markdown += "- $cycle"
        }
        $markdown += ""
    }
    
    # Save visualization
    $markdown -join "`n" | Out-File -FilePath $VISUALIZATION_FILE -Encoding UTF8
    Write-Host "📊 Visualization saved to $VISUALIZATION_FILE" -ForegroundColor Green
    
    # Quick console summary
    Write-Host ""
    Write-Host "📊 DEPENDENCY MAP SUMMARY" -ForegroundColor Cyan
    Write-Host "=========================" -ForegroundColor Cyan
    Write-Host "📁 Files analyzed: $($Map.metadata.total_files)" -ForegroundColor White
    Write-Host "🔧 Functions found: $($Map.metadata.total_functions)" -ForegroundColor White
    Write-Host "🔗 Dependencies: $($Map.metadata.total_dependencies)" -ForegroundColor White
    
    if ($Map.orphaned_functions.Count -gt 0) {
        Write-Host "🥀 Orphaned functions: $($Map.orphaned_functions.Count)" -ForegroundColor Yellow
        Write-Host "   └─ $($Map.orphaned_functions[0..2] -join ', ')$(if ($Map.orphaned_functions.Count -gt 3) { '...' })" -ForegroundColor Gray
    }
    
    if ($Map.circular_dependencies.Count -gt 0) {
        Write-Host "🔄 Circular dependencies: $($Map.circular_dependencies.Count)" -ForegroundColor Red
    }
    
    if ($Map.external_packages.Count -gt 0) {
        Write-Host "📦 External packages: $($Map.external_packages.Keys -join ', ')" -ForegroundColor Cyan
    }
}

# Main execution
if ($Event -eq "post_git_commit") {
    Write-Host ""
    Write-Host "🚀 PACKAGE DEPENDENCY MAPPER TRIGGERED" -ForegroundColor Green
    Write-Host "=======================================" -ForegroundColor Green
    Write-Host "Commit: $CommitHash" -ForegroundColor White
    Write-Host "Message: $CommitMessage" -ForegroundColor White
    Write-Host ""
    
    New-PackageDependencyMap
    
    Write-Host ""
    Write-Host "🎉 Package dependency mapping complete!" -ForegroundColor Green
    Write-Host "📁 Check .claude/ directory for detailed maps and reports" -ForegroundColor Cyan
    Write-Host ""
} { "object property assignment" }
                    '\\?\s*\(' { "anonymous function" }
                    default { "unknown pattern" }
                }
                
                Write-Host "    🔍 Found '$functionName' ($patternType)" -ForegroundColor Gray
            }
        }
        
        if ($fileFunctions.Count -gt 0) {
            $definitions[$file] = $fileFunctions
            Write-Host "    ✅ Found $($fileFunctions.Count) functions: $($fileFunctions -join ', ')" -ForegroundColor Green
        }
    }
    
    return @{
        functions = $functions
        definitions = $definitions
    }
}

function Get-FunctionDependencies {
    param(
        [string[]]$Files,
        [hashtable]$Functions
    )
    
    $internalDeps = @{}
    $externalDeps = @{}
    
    foreach ($file in $Files) {
        $content = Get-Content $file -Raw
        
        # Get all function calls in this file
        $functionCalls = Get-FunctionCallsFromContent -Content $content
        
        # Get functions defined in this file
        $fileFunctions = $Functions.GetEnumerator() | Where-Object { $_.Value -eq $file } | ForEach-Object { $_.Key }
        
        # For each function in this file, determine what it calls
        foreach ($func in $fileFunctions) {
            $funcPattern = "^\s*$func\s*<-\s*function\s*\("
            $funcStart = $content | Select-String -Pattern $funcPattern
            
            if ($funcStart) {
                # Extract the function body (simplified - gets content after function definition)
                $lines = $content -split "`n"
                $startLine = $funcStart.LineNumber - 1
                $funcBody = ""
                $braceCount = 0
                $inFunction = $false
                
                for ($i = $startLine; $i -lt $lines.Length; $i++) {
                    $line = $lines[$i]
                    if ($line -match '\{') { 
                        $braceCount += ($line.ToCharArray() | Where-Object { $_ -eq '{' }).Count
                        $inFunction = $true
                    }
                    if ($inFunction) {
                        $funcBody += $line + "`n"
                    }
                    if ($line -match '\}') {
                        $braceCount -= ($line.ToCharArray() | Where-Object { $_ -eq '}' }).Count
                        if ($braceCount -le 0 -and $inFunction) { break }
                    }
                }
                
                # Find function calls within this function body
                $funcCalls = Get-FunctionCallsFromContent -Content $funcBody
                
                # Separate internal vs external calls
                $internalCalls = @()
                foreach ($call in $funcCalls) {
                    if ($call -match '::') {
                        # External package call
                        $parts = $call -split '::'
                        $package = $parts[0]
                        $function = $parts[1]
                        
                        if (-not $externalDeps.ContainsKey($package)) {
                            $externalDeps[$package] = @()
                        }
                        if ($function -notin $externalDeps[$package]) {
                            $externalDeps[$package] += $function
                        }
                    }
                    elseif ($Functions.ContainsKey($call) -and $call -ne $func) {
                        # Internal function call (exclude self-calls)
                        $internalCalls += $call
                    }
                }
                
                if ($internalCalls.Count -gt 0) {
                    $internalDeps[$func] = $internalCalls | Sort-Object -Unique
                }
            }
        }
    }
    
    return @{
        internal = $internalDeps
        external = $externalDeps
    }
}

function Get-FunctionCallsFromContent {
    param([string]$Content)
    
    $functionCalls = @()
    
    # Enhanced patterns for function calls
    $patterns = @(
        '\b([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Standard calls: func()
        '([a-zA-Z_][a-zA-Z0-9_.]+)::\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Package calls: pkg::func()
        '`([^`]+)`\s*\('  # Backtick functions: `%>%`()
    )
    
    foreach ($pattern in $patterns) {
        $matches = [regex]::Matches($Content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
        foreach ($match in $matches) {
            if ($pattern -match '::') {
                # Package::function format
                $functionCalls += "$($match.Groups[1].Value)::$($match.Groups[2].Value)"
            } else {
                # Regular function
                $funcName = $match.Groups[1].Value
                # Exclude base R functions
                if ($funcName -notin $BASE_FUNCTIONS) {
                    $functionCalls += $funcName
                }
            }
        }
    }
    
    return $functionCalls | Sort-Object -Unique
}

function Get-ReverseDependencies {
    param([hashtable]$Dependencies)
    
    $reverseDeps = @{}
    
    # Initialize all functions with empty arrays
    foreach ($func in $Dependencies.Keys) {
        if (-not $reverseDeps.ContainsKey($func)) {
            $reverseDeps[$func] = @()
        }
        
        foreach ($calledFunc in $Dependencies[$func]) {
            if (-not $reverseDeps.ContainsKey($calledFunc)) {
                $reverseDeps[$calledFunc] = @()
            }
            $reverseDeps[$calledFunc] += $func
        }
    }
    
    # Remove duplicates
    foreach ($func in $reverseDeps.Keys) {
        $reverseDeps[$func] = $reverseDeps[$func] | Sort-Object -Unique
    }
    
    return $reverseDeps
}

function Get-FileDependencies {
    param(
        [hashtable]$FunctionDeps,
        [hashtable]$FunctionLocations
    )
    
    $fileDeps = @{}
    
    foreach ($func in $FunctionDeps.Keys) {
        $funcFile = $FunctionLocations[$func]
        if (-not $funcFile) { continue }
        
        if (-not $fileDeps.ContainsKey($funcFile)) {
            $fileDeps[$funcFile] = @()
        }
        
        foreach ($calledFunc in $FunctionDeps[$func]) {
            $calledFile = $FunctionLocations[$calledFunc]
            if ($calledFile -and $calledFile -ne $funcFile) {
                $fileDeps[$funcFile] += $calledFile
            }
        }
        
        # Remove duplicates
        $fileDeps[$funcFile] = $fileDeps[$funcFile] | Sort-Object -Unique
    }
    
    return $fileDeps
}

function Get-OrphanedFunctions {
    param(
        [hashtable]$Functions,
        [hashtable]$ReverseDeps
    )
    
    $orphaned = @()
    
    foreach ($func in $Functions.Keys) {
        if (-not $ReverseDeps.ContainsKey($func) -or $ReverseDeps[$func].Count -eq 0) {
            $orphaned += $func
        }
    }
    
    return $orphaned | Sort-Object
}

function Get-CircularDependencies {
    param([hashtable]$Dependencies)
    
    $circular = @()
    $visited = @{}
    $recursionStack = @{}
    
    function Find-CircularPath {
        param([string]$Function, [string[]]$Path)
        
        if ($recursionStack.ContainsKey($Function)) {
            # Found a cycle - extract the circular part
            $cycleStart = [array]::IndexOf($Path, $Function)
            if ($cycleStart -ge 0) {
                $cycle = $Path[$cycleStart..($Path.Length-1)] + $Function
                return $cycle
            }
        }
        
        if ($visited.ContainsKey($Function)) {
            return $null
        }
        
        $visited[$Function] = $true
        $recursionStack[$Function] = $true
        $newPath = $Path + $Function
        
        if ($Dependencies.ContainsKey($Function)) {
            foreach ($calledFunc in $Dependencies[$Function]) {
                $cycle = Find-CircularPath -Function $calledFunc -Path $newPath
                if ($cycle) {
                    return $cycle
                }
            }
        }
        
        $recursionStack.Remove($Function)
        return $null
    }
    
    foreach ($func in $Dependencies.Keys) {
        if (-not $visited.ContainsKey($func)) {
            $cycle = Find-CircularPath -Function $func -Path @()
            if ($cycle) {
                $circular += ($cycle -join " → ")
            }
        }
    }
    
    return $circular | Sort-Object -Unique
}

function Save-DependencyMap {
    param([hashtable]$Map)
    
    # Ensure .claude directory exists
    if (-not (Test-Path ".claude")) {
        New-Item -ItemType Directory -Path ".claude" -Force | Out-Null
    }
    
    # Save main dependency map
    $Map | ConvertTo-Json -Depth 10 | Out-File -FilePath $CACHE_FILE -Encoding UTF8
    
    # Save metadata separately for quick access
    $Map.metadata | ConvertTo-Json -Depth 3 | Out-File -FilePath $METADATA_FILE -Encoding UTF8
    
    Write-Host "💾 Dependency map saved to $CACHE_FILE" -ForegroundColor Green
    Write-Host "📋 Metadata saved to $METADATA_FILE" -ForegroundColor Green
}

function New-DependencyReports {
    param([hashtable]$Map)
    
    # Generate markdown visualization
    $markdown = @()
    $markdown += "# Package Dependency Map"
    $markdown += ""
    $markdown += "Generated: $($Map.metadata.generated)"
    $markdown += "Package: $($Map.metadata.package_name) v$($Map.metadata.version)"
    $markdown += "Commit: $($Map.metadata.commit_hash)"
    $markdown += ""
    
    # Summary statistics
    $markdown += "## Summary Statistics"
    $markdown += ""
    $markdown += "- **Total Files**: $($Map.metadata.total_files)"
    $markdown += "- **Total Functions**: $($Map.metadata.total_functions)"
    $markdown += "- **Total Dependencies**: $($Map.metadata.total_dependencies)"
    $markdown += "- **Orphaned Functions**: $($Map.orphaned_functions.Count)"
    $markdown += "- **Circular Dependencies**: $($Map.circular_dependencies.Count)"
    $markdown += "- **External Packages**: $($Map.external_packages.Count)"
    $markdown += ""
    
    # File overview
    if ($Map.definitions.Count -gt 0) {
        $markdown += "## File Overview"
        $markdown += ""
        foreach ($file in $Map.definitions.Keys | Sort-Object) {
            $funcs = $Map.definitions[$file]
            $fileSize = if (Test-Path $file) { [math]::Round((Get-Item $file).Length / 1024, 1) } else { "?" }
            $markdown += "### $file ($fileSize KB)"
            $markdown += ""
            $markdown += "Functions: $($funcs -join ', ')"
            $markdown += ""
        }
    }
    
    # External packages
    if ($Map.external_packages.Count -gt 0) {
        $markdown += "## External Package Dependencies"
        $markdown += ""
        foreach ($pkg in $Map.external_packages.Keys | Sort-Object) {
            $funcs = $Map.external_packages[$pkg] -join ', '
            $markdown += "- **$pkg**: $funcs"
        }
        $markdown += ""
    }
    
    # Orphaned functions
    if ($Map.orphaned_functions.Count -gt 0) {
        $markdown += "## Orphaned Functions"
        $markdown += ""
        $markdown += "Functions with no callers (potential candidates for removal):"
        $markdown += ""
        foreach ($func in $Map.orphaned_functions) {
            $file = $Map.functions[$func]
            $markdown += "- **$func** (in $file)"
        }
        $markdown += ""
    }
    
    # Circular dependencies
    if ($Map.circular_dependencies.Count -gt 0) {
        $markdown += "## Circular Dependencies"
        $markdown += ""
        $markdown += "⚠️ These function chains have circular dependencies:"
        $markdown += ""
        foreach ($cycle in $Map.circular_dependencies) {
            $markdown += "- $cycle"
        }
        $markdown += ""
    }
    
    # Save visualization
    $markdown -join "`n" | Out-File -FilePath $VISUALIZATION_FILE -Encoding UTF8
    Write-Host "📊 Visualization saved to $VISUALIZATION_FILE" -ForegroundColor Green
    
    # Quick console summary
    Write-Host ""
    Write-Host "📊 DEPENDENCY MAP SUMMARY" -ForegroundColor Cyan
    Write-Host "=========================" -ForegroundColor Cyan
    Write-Host "📁 Files analyzed: $($Map.metadata.total_files)" -ForegroundColor White
    Write-Host "🔧 Functions found: $($Map.metadata.total_functions)" -ForegroundColor White
    Write-Host "🔗 Dependencies: $($Map.metadata.total_dependencies)" -ForegroundColor White
    
    if ($Map.orphaned_functions.Count -gt 0) {
        Write-Host "🥀 Orphaned functions: $($Map.orphaned_functions.Count)" -ForegroundColor Yellow
        Write-Host "   └─ $($Map.orphaned_functions[0..2] -join ', ')$(if ($Map.orphaned_functions.Count -gt 3) { '...' })" -ForegroundColor Gray
    }
    
    if ($Map.circular_dependencies.Count -gt 0) {
        Write-Host "🔄 Circular dependencies: $($Map.circular_dependencies.Count)" -ForegroundColor Red
    }
    
    if ($Map.external_packages.Count -gt 0) {
        Write-Host "📦 External packages: $($Map.external_packages.Keys -join ', ')" -ForegroundColor Cyan
    }
}

# Main execution
if ($Event -eq "post_git_commit") {
    Write-Host ""
    Write-Host "🚀 PACKAGE DEPENDENCY MAPPER TRIGGERED" -ForegroundColor Green
    Write-Host "=======================================" -ForegroundColor Green
    Write-Host "Commit: $CommitHash" -ForegroundColor White
    Write-Host "Message: $CommitMessage" -ForegroundColor White
    Write-Host ""
    
    New-PackageDependencyMap
    
    Write-Host ""
    Write-Host "🎉 Package dependency mapping complete!" -ForegroundColor Green
    Write-Host "📁 Check .claude/ directory for detailed maps and reports" -ForegroundColor Cyan
    Write-Host ""
}    # name <- function (on separate line)
        )
        
        foreach ($pattern in $patterns) {
            $matches = [regex]::Matches($content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
            foreach ($match in $matches) {
                $functionName = $match.Groups[1].Value
                
                # Skip if already found (avoid duplicates)
                if ($functions.ContainsKey($functionName)) {
                    Write-Host "    ⚠️  Duplicate function '$functionName' found in $file (already in $($functions[$functionName]))" -ForegroundColor Yellow
                    continue
                }
                
                $functions[$functionName] = $file
                $fileFunctions += $functionName
            }
        }
        
        if ($fileFunctions.Count -gt 0) {
            $definitions[$file] = $fileFunctions
            Write-Host "    ✅ Found $($fileFunctions.Count) functions: $($fileFunctions -join ', ')" -ForegroundColor Green
        }
    }
    
    return @{
        functions = $functions
        definitions = $definitions
    }
}

function Get-FunctionDependencies {
    param(
        [string[]]$Files,
        [hashtable]$Functions
    )
    
    $internalDeps = @{}
    $externalDeps = @{}
    
    foreach ($file in $Files) {
        $content = Get-Content $file -Raw
        
        # Get all function calls in this file
        $functionCalls = Get-FunctionCallsFromContent -Content $content
        
        # Get functions defined in this file
        $fileFunctions = $Functions.GetEnumerator() | Where-Object { $_.Value -eq $file } | ForEach-Object { $_.Key }
        
        # For each function in this file, determine what it calls
        foreach ($func in $fileFunctions) {
            $funcPattern = "^\s*$func\s*<-\s*function\s*\("
            $funcStart = $content | Select-String -Pattern $funcPattern
            
            if ($funcStart) {
                # Extract the function body (simplified - gets content after function definition)
                $lines = $content -split "`n"
                $startLine = $funcStart.LineNumber - 1
                $funcBody = ""
                $braceCount = 0
                $inFunction = $false
                
                for ($i = $startLine; $i -lt $lines.Length; $i++) {
                    $line = $lines[$i]
                    if ($line -match '\{') { 
                        $braceCount += ($line.ToCharArray() | Where-Object { $_ -eq '{' }).Count
                        $inFunction = $true
                    }
                    if ($inFunction) {
                        $funcBody += $line + "`n"
                    }
                    if ($line -match '\}') {
                        $braceCount -= ($line.ToCharArray() | Where-Object { $_ -eq '}' }).Count
                        if ($braceCount -le 0 -and $inFunction) { break }
                    }
                }
                
                # Find function calls within this function body
                $funcCalls = Get-FunctionCallsFromContent -Content $funcBody
                
                # Separate internal vs external calls
                $internalCalls = @()
                foreach ($call in $funcCalls) {
                    if ($call -match '::') {
                        # External package call
                        $parts = $call -split '::'
                        $package = $parts[0]
                        $function = $parts[1]
                        
                        if (-not $externalDeps.ContainsKey($package)) {
                            $externalDeps[$package] = @()
                        }
                        if ($function -notin $externalDeps[$package]) {
                            $externalDeps[$package] += $function
                        }
                    }
                    elseif ($Functions.ContainsKey($call) -and $call -ne $func) {
                        # Internal function call (exclude self-calls)
                        $internalCalls += $call
                    }
                }
                
                if ($internalCalls.Count -gt 0) {
                    $internalDeps[$func] = $internalCalls | Sort-Object -Unique
                }
            }
        }
    }
    
    return @{
        internal = $internalDeps
        external = $externalDeps
    }
}

function Get-FunctionCallsFromContent {
    param([string]$Content)
    
    $functionCalls = @()
    
    # Enhanced patterns for function calls
    $patterns = @(
        '\b([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Standard calls: func()
        '([a-zA-Z_][a-zA-Z0-9_.]+)::\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*\(',  # Package calls: pkg::func()
        '`([^`]+)`\s*\('  # Backtick functions: `%>%`()
    )
    
    foreach ($pattern in $patterns) {
        $matches = [regex]::Matches($Content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
        foreach ($match in $matches) {
            if ($pattern -match '::') {
                # Package::function format
                $functionCalls += "$($match.Groups[1].Value)::$($match.Groups[2].Value)"
            } else {
                # Regular function
                $funcName = $match.Groups[1].Value
                # Exclude base R functions
                if ($funcName -notin $BASE_FUNCTIONS) {
                    $functionCalls += $funcName
                }
            }
        }
    }
    
    return $functionCalls | Sort-Object -Unique
}

function Get-ReverseDependencies {
    param([hashtable]$Dependencies)
    
    $reverseDeps = @{}
    
    # Initialize all functions with empty arrays
    foreach ($func in $Dependencies.Keys) {
        if (-not $reverseDeps.ContainsKey($func)) {
            $reverseDeps[$func] = @()
        }
        
        foreach ($calledFunc in $Dependencies[$func]) {
            if (-not $reverseDeps.ContainsKey($calledFunc)) {
                $reverseDeps[$calledFunc] = @()
            }
            $reverseDeps[$calledFunc] += $func
        }
    }
    
    # Remove duplicates
    foreach ($func in $reverseDeps.Keys) {
        $reverseDeps[$func] = $reverseDeps[$func] | Sort-Object -Unique
    }
    
    return $reverseDeps
}

function Get-FileDependencies {
    param(
        [hashtable]$FunctionDeps,
        [hashtable]$FunctionLocations
    )
    
    $fileDeps = @{}
    
    foreach ($func in $FunctionDeps.Keys) {
        $funcFile = $FunctionLocations[$func]
        if (-not $funcFile) { continue }
        
        if (-not $fileDeps.ContainsKey($funcFile)) {
            $fileDeps[$funcFile] = @()
        }
        
        foreach ($calledFunc in $FunctionDeps[$func]) {
            $calledFile = $FunctionLocations[$calledFunc]
            if ($calledFile -and $calledFile -ne $funcFile) {
                $fileDeps[$funcFile] += $calledFile
            }
        }
        
        # Remove duplicates
        $fileDeps[$funcFile] = $fileDeps[$funcFile] | Sort-Object -Unique
    }
    
    return $fileDeps
}

function Get-OrphanedFunctions {
    param(
        [hashtable]$Functions,
        [hashtable]$ReverseDeps
    )
    
    $orphaned = @()
    
    foreach ($func in $Functions.Keys) {
        if (-not $ReverseDeps.ContainsKey($func) -or $ReverseDeps[$func].Count -eq 0) {
            $orphaned += $func
        }
    }
    
    return $orphaned | Sort-Object
}

function Get-CircularDependencies {
    param([hashtable]$Dependencies)
    
    $circular = @()
    $visited = @{}
    $recursionStack = @{}
    
    function Find-CircularPath {
        param([string]$Function, [string[]]$Path)
        
        if ($recursionStack.ContainsKey($Function)) {
            # Found a cycle - extract the circular part
            $cycleStart = [array]::IndexOf($Path, $Function)
            if ($cycleStart -ge 0) {
                $cycle = $Path[$cycleStart..($Path.Length-1)] + $Function
                return $cycle
            }
        }
        
        if ($visited.ContainsKey($Function)) {
            return $null
        }
        
        $visited[$Function] = $true
        $recursionStack[$Function] = $true
        $newPath = $Path + $Function
        
        if ($Dependencies.ContainsKey($Function)) {
            foreach ($calledFunc in $Dependencies[$Function]) {
                $cycle = Find-CircularPath -Function $calledFunc -Path $newPath
                if ($cycle) {
                    return $cycle
                }
            }
        }
        
        $recursionStack.Remove($Function)
        return $null
    }
    
    foreach ($func in $Dependencies.Keys) {
        if (-not $visited.ContainsKey($func)) {
            $cycle = Find-CircularPath -Function $func -Path @()
            if ($cycle) {
                $circular += ($cycle -join " → ")
            }
        }
    }
    
    return $circular | Sort-Object -Unique
}

function Save-DependencyMap {
    param([hashtable]$Map)
    
    # Ensure .claude directory exists
    if (-not (Test-Path ".claude")) {
        New-Item -ItemType Directory -Path ".claude" -Force | Out-Null
    }
    
    # Save main dependency map
    $Map | ConvertTo-Json -Depth 10 | Out-File -FilePath $CACHE_FILE -Encoding UTF8
    
    # Save metadata separately for quick access
    $Map.metadata | ConvertTo-Json -Depth 3 | Out-File -FilePath $METADATA_FILE -Encoding UTF8
    
    Write-Host "💾 Dependency map saved to $CACHE_FILE" -ForegroundColor Green
    Write-Host "📋 Metadata saved to $METADATA_FILE" -ForegroundColor Green
}

function New-DependencyReports {
    param([hashtable]$Map)
    
    # Generate markdown visualization
    $markdown = @()
    $markdown += "# Package Dependency Map"
    $markdown += ""
    $markdown += "Generated: $($Map.metadata.generated)"
    $markdown += "Package: $($Map.metadata.package_name) v$($Map.metadata.version)"
    $markdown += "Commit: $($Map.metadata.commit_hash)"
    $markdown += ""
    
    # Summary statistics
    $markdown += "## Summary Statistics"
    $markdown += ""
    $markdown += "- **Total Files**: $($Map.metadata.total_files)"
    $markdown += "- **Total Functions**: $($Map.metadata.total_functions)"
    $markdown += "- **Total Dependencies**: $($Map.metadata.total_dependencies)"
    $markdown += "- **Orphaned Functions**: $($Map.orphaned_functions.Count)"
    $markdown += "- **Circular Dependencies**: $($Map.circular_dependencies.Count)"
    $markdown += "- **External Packages**: $($Map.external_packages.Count)"
    $markdown += ""
    
    # File overview
    if ($Map.definitions.Count -gt 0) {
        $markdown += "## File Overview"
        $markdown += ""
        foreach ($file in $Map.definitions.Keys | Sort-Object) {
            $funcs = $Map.definitions[$file]
            $fileSize = if (Test-Path $file) { [math]::Round((Get-Item $file).Length / 1024, 1) } else { "?" }
            $markdown += "### $file ($fileSize KB)"
            $markdown += ""
            $markdown += "Functions: $($funcs -join ', ')"
            $markdown += ""
        }
    }
    
    # External packages
    if ($Map.external_packages.Count -gt 0) {
        $markdown += "## External Package Dependencies"
        $markdown += ""
        foreach ($pkg in $Map.external_packages.Keys | Sort-Object) {
            $funcs = $Map.external_packages[$pkg] -join ', '
            $markdown += "- **$pkg**: $funcs"
        }
        $markdown += ""
    }
    
    # Orphaned functions
    if ($Map.orphaned_functions.Count -gt 0) {
        $markdown += "## Orphaned Functions"
        $markdown += ""
        $markdown += "Functions with no callers (potential candidates for removal):"
        $markdown += ""
        foreach ($func in $Map.orphaned_functions) {
            $file = $Map.functions[$func]
            $markdown += "- **$func** (in $file)"
        }
        $markdown += ""
    }
    
    # Circular dependencies
    if ($Map.circular_dependencies.Count -gt 0) {
        $markdown += "## Circular Dependencies"
        $markdown += ""
        $markdown += "⚠️ These function chains have circular dependencies:"
        $markdown += ""
        foreach ($cycle in $Map.circular_dependencies) {
            $markdown += "- $cycle"
        }
        $markdown += ""
    }
    
    # Save visualization
    $markdown -join "`n" | Out-File -FilePath $VISUALIZATION_FILE -Encoding UTF8
    Write-Host "📊 Visualization saved to $VISUALIZATION_FILE" -ForegroundColor Green
    
    # Quick console summary
    Write-Host ""
    Write-Host "📊 DEPENDENCY MAP SUMMARY" -ForegroundColor Cyan
    Write-Host "=========================" -ForegroundColor Cyan
    Write-Host "📁 Files analyzed: $($Map.metadata.total_files)" -ForegroundColor White
    Write-Host "🔧 Functions found: $($Map.metadata.total_functions)" -ForegroundColor White
    Write-Host "🔗 Dependencies: $($Map.metadata.total_dependencies)" -ForegroundColor White
    
    if ($Map.orphaned_functions.Count -gt 0) {
        Write-Host "🥀 Orphaned functions: $($Map.orphaned_functions.Count)" -ForegroundColor Yellow
        Write-Host "   └─ $($Map.orphaned_functions[0..2] -join ', ')$(if ($Map.orphaned_functions.Count -gt 3) { '...' })" -ForegroundColor Gray
    }
    
    if ($Map.circular_dependencies.Count -gt 0) {
        Write-Host "🔄 Circular dependencies: $($Map.circular_dependencies.Count)" -ForegroundColor Red
    }
    
    if ($Map.external_packages.Count -gt 0) {
        Write-Host "📦 External packages: $($Map.external_packages.Keys -join ', ')" -ForegroundColor Cyan
    }
}

# Main execution
if ($Event -eq "post_git_commit") {
    Write-Host ""
    Write-Host "🚀 PACKAGE DEPENDENCY MAPPER TRIGGERED" -ForegroundColor Green
    Write-Host "=======================================" -ForegroundColor Green
    Write-Host "Commit: $CommitHash" -ForegroundColor White
    Write-Host "Message: $CommitMessage" -ForegroundColor White
    Write-Host ""
    
    New-PackageDependencyMap
    
    Write-Host ""
    Write-Host "🎉 Package dependency mapping complete!" -ForegroundColor Green
    Write-Host "📁 Check .claude/ directory for detailed maps and reports" -ForegroundColor Cyan
    Write-Host ""
}
