#!/bin/bash
# Package-Wide Function Dependency Mapper Hook - Bash Version
# Optimized for Linux/macOS with caching support

# Configuration
CACHE_FILE="architecture/package-dependency-map.json"
METADATA_FILE="architecture/dependency-metadata.json"
VISUALIZATION_FILE="architecture/dependency-graph.md"
FILE_CACHE_DIR="architecture/file-cache"
FILE_METADATA="$FILE_CACHE_DIR/file-metadata.json"

# Ensure directories exist
mkdir -p "$FILE_CACHE_DIR"

# Get file hash (size:mtime) for cache invalidation
get_file_hash() {
    local file="$1"
    if [[ -f "$file" ]]; then
        stat -c "%s:%Y" "$file" 2>/dev/null || stat -f "%z:%m" "$file" 2>/dev/null || echo ""
    fi
}

# Get package info from DESCRIPTION
get_package_name() {
    grep "^Package:" DESCRIPTION 2>/dev/null | sed 's/^Package:[[:space:]]*//' || echo "unknown"
}

get_package_version() {
    grep "^Version:" DESCRIPTION 2>/dev/null | sed 's/^Version:[[:space:]]*//' || echo "0.0.0"
}

# Get external dependencies from DESCRIPTION
get_external_dependencies() {
    if [[ -f "DESCRIPTION" ]]; then
        awk '
            /^Imports:|^Depends:/ { capture=1; next }
            /^[A-Z][a-z]+:/ { capture=0 }
            capture { gsub(/[[:space:]]/, ""); print }
        ' DESCRIPTION | tr ',' '\n' | sed 's/([^)]*)//g' | grep -v "^R$" | grep -v "^$" | sort -u
    fi
}

# Get exported functions from NAMESPACE
get_exported_functions() {
    if [[ -f "NAMESPACE" ]]; then
        grep "^export(" NAMESPACE | sed 's/export(\(.*\))/\1/' | tr -d '"' | sort -u
    fi
}

# Find all R files
get_all_r_files() {
    find R -name "*.R" -type f 2>/dev/null | sort
}

# Extract function definitions from an R file
extract_functions_from_file() {
    local file="$1"
    grep -oE "^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_.]*[[:space:]]*(<-|=)[[:space:]]*function[[:space:]]*\(" "$file" 2>/dev/null | \
        sed -E 's/^[[:space:]]*([a-zA-Z_][a-zA-Z0-9_.]*)[[:space:]]*(<-|=).*/\1/' | sort -u || true
}

# Detect S3 methods
detect_s3_methods() {
    local file="$1"
    grep -oE "^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_]*\.[a-zA-Z_][a-zA-Z0-9_.]*[[:space:]]*(<-|=)[[:space:]]*function" "$file" 2>/dev/null | \
        sed -E 's/^[[:space:]]*([a-zA-Z_][a-zA-Z0-9_]*\.[a-zA-Z_][a-zA-Z0-9_.]*).*/\1/' | sort -u || true
}

# Get file category based on name
categorize_file() {
    local file="$1"
    local basename
    basename=$(basename "$file" .R)

    case "$basename" in
        mvgam|mvgam_core|sim_mvgam|series_to_mvgam) echo "Core" ;;
        *plot*) echo "Plotting" ;;
        *stan*|*brms*|make_stan) echo "Stan/Modeling" ;;
        *valid*|*check*) echo "Validation" ;;
        *print*|*summary*|*predict*|*resid*|*fitted*) echo "S3 Methods" ;;
        *util*|*helper*|globals) echo "Utilities" ;;
        *data*|portal_data|all_neon*) echo "Data" ;;
        *) echo "Other" ;;
    esac
}

# Extract function body and find calls to internal functions
extract_function_calls() {
    local file="$1"
    local func_name="$2"
    local all_funcs="$3"

    # Extract function body using awk
    local body
    body=$(awk -v fname="$func_name" '
        BEGIN { in_func=0; brace_count=0; started=0 }
        $0 ~ "^[[:space:]]*" fname "[[:space:]]*(<-|=)[[:space:]]*function" {
            in_func=1
            started=1
        }
        in_func {
            brace_count += gsub(/{/, "{")
            brace_count -= gsub(/}/, "}")
            print
            if (started && brace_count == 0 && /}/) exit
        }
    ' "$file" 2>/dev/null)

    if [[ -z "$body" ]]; then
        return
    fi

    # Find all potential function calls in the body
    local calls
    calls=$(echo "$body" | grep -oE "[a-zA-Z_][a-zA-Z0-9_.]*[[:space:]]*\(" | sed 's/[[:space:]]*($//' | sort -u)

    # Filter to only internal functions
    local result=""
    while IFS= read -r call; do
        [[ -z "$call" ]] && continue
        [[ "$call" == "$func_name" ]] && continue  # Skip self-calls
        if echo "$all_funcs" | grep -qx "$call"; then
            result+="$call "
        fi
    done <<< "$calls"

    echo "$result" | tr ' ' '\n' | grep -v "^$" | sort -u | tr '\n' ',' | sed 's/,$//'
}

# Build dependency maps for priority files
build_dependency_maps() {
    local r_files="$1"
    local all_funcs="$2"

    # Priority files for detailed dependency analysis
    local priority_pattern="mvgam_core|stan_assembly|brms_integration|priors|trend_system|validations|predictions"

    # Forward dependencies: func -> [calls]
    local forward_deps="{}"
    # Reverse dependencies: func -> [called_by]
    local reverse_deps="{}"

    echo "Building dependency maps for priority files..." >&2

    while IFS= read -r file; do
        [[ -z "$file" ]] && continue

        # Only analyze priority files for dependencies
        if ! echo "$file" | grep -qE "$priority_pattern"; then
            continue
        fi

        local funcs
        funcs=$(extract_functions_from_file "$file")

        while IFS= read -r func; do
            [[ -z "$func" ]] && continue

            local calls
            calls=$(extract_function_calls "$file" "$func" "$all_funcs")

            if [[ -n "$calls" ]]; then
                # Add to forward deps
                forward_deps=$(echo "$forward_deps" | jq --arg f "$func" --arg c "$calls" '.[$f] = $c')

                # Build reverse deps
                IFS=',' read -ra call_array <<< "$calls"
                for called in "${call_array[@]}"; do
                    [[ -z "$called" ]] && continue
                    local existing
                    existing=$(echo "$reverse_deps" | jq -r --arg f "$called" '.[$f] // ""')
                    if [[ -z "$existing" ]]; then
                        reverse_deps=$(echo "$reverse_deps" | jq --arg f "$called" --arg c "$func" '.[$f] = $c')
                    else
                        reverse_deps=$(echo "$reverse_deps" | jq --arg f "$called" --arg c "$existing,$func" '.[$f] = $c')
                    fi
                done
            fi
        done <<< "$funcs"
    done <<< "$r_files"

    # Output as JSON object
    echo "{\"forward\": $forward_deps, \"reverse\": $reverse_deps}"
}

# Main analysis function
run_analysis() {
    local commit_hash="${1:-$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')}"
    local commit_message="${2:-$(git log -1 --pretty=%B 2>/dev/null | head -1 || echo 'pre-push analysis')}"

    echo "Running package dependency analysis..." >&2

    # Get all R files
    local r_files
    r_files=$(get_all_r_files)
    local total_files
    total_files=$(echo "$r_files" | grep -c . 2>/dev/null | head -1 || echo 0)
    total_files=${total_files:-0}

    # Count changed vs cached files
    local changed_count=0
    local cached_count=0

    if [[ -f "$FILE_METADATA" ]]; then
        while IFS= read -r file; do
            [[ -z "$file" ]] && continue
            local current_hash
            current_hash=$(get_file_hash "$file")
            local cached_hash
            cached_hash=$(jq -r --arg f "$file" '.[$f] // ""' "$FILE_METADATA" 2>/dev/null || echo "")
            if [[ "$current_hash" != "$cached_hash" ]]; then
                ((changed_count++))
            else
                ((cached_count++))
            fi
        done <<< "$r_files"
    else
        changed_count=$total_files
    fi

    echo "Processing $total_files files ($changed_count changed, $cached_count cached)..." >&2

    # Get exported functions
    local exported_funcs
    exported_funcs=$(get_exported_functions)
    local exported_count
    exported_count=$(echo "$exported_funcs" | grep -c . 2>/dev/null | head -1 || echo 0)
    exported_count=${exported_count:-0}

    # Get external dependencies
    local external_deps
    external_deps=$(get_external_dependencies)

    # Count all functions and build file metadata cache
    local total_functions=0
    local file_hashes="{}"

    while IFS= read -r file; do
        [[ -z "$file" ]] && continue
        local funcs
        funcs=$(extract_functions_from_file "$file")
        local func_count=0
        if [[ -n "$funcs" ]]; then
            func_count=$(echo "$funcs" | wc -l | tr -d ' ')
        fi
        total_functions=$((total_functions + func_count))

        # Update file hash
        local hash
        hash=$(get_file_hash "$file")
        file_hashes=$(echo "$file_hashes" | jq --arg f "$file" --arg h "$hash" '.[$f] = $h')
    done <<< "$r_files"

    # Save file metadata
    echo "$file_hashes" > "$FILE_METADATA"

    # Count S3 methods
    local s3_method_count=0
    local all_s3_classes=""
    while IFS= read -r file; do
        [[ -z "$file" ]] && continue
        local s3s
        s3s=$(detect_s3_methods "$file")
        while IFS= read -r s3; do
            [[ -z "$s3" ]] && continue
            if [[ "$s3" == *.* ]]; then
                ((s3_method_count++))
                local class="${s3#*.}"
                all_s3_classes="$all_s3_classes $class"
            fi
        done <<< "$s3s"
    done <<< "$r_files"

    local s3_class_count
    s3_class_count=$(echo "$all_s3_classes" | tr ' ' '\n' | sort -u | grep -c . 2>/dev/null | head -1 || echo 0)
    s3_class_count=${s3_class_count:-0}

    local internal_count=$((total_functions - exported_count))
    local generated_time
    generated_time=$(date "+%Y-%m-%d %H:%M:%S")

    # Build all functions list for dependency analysis
    local all_funcs=""
    while IFS= read -r file; do
        [[ -z "$file" ]] && continue
        local funcs
        funcs=$(extract_functions_from_file "$file")
        all_funcs+="$funcs"$'\n'
    done <<< "$r_files"
    all_funcs=$(echo "$all_funcs" | sort -u | grep -v "^$")

    # Build dependency maps
    local dep_maps
    dep_maps=$(build_dependency_maps "$r_files" "$all_funcs")

    # Extract forward and reverse deps
    local forward_deps reverse_deps
    forward_deps=$(echo "$dep_maps" | jq -r '.forward')
    reverse_deps=$(echo "$dep_maps" | jq -r '.reverse')

    # Count functions with dependencies
    local deps_count
    deps_count=$(echo "$forward_deps" | jq 'keys | length')

    # Generate metadata JSON
    cat > "$METADATA_FILE" << EOF
{
    "generated": "$generated_time",
    "commit_hash": "$commit_hash",
    "commit_message": "$(echo "$commit_message" | sed 's/"/\\"/g')",
    "package_name": "$(get_package_name)",
    "version": "$(get_package_version)",
    "total_files": $total_files,
    "total_functions": $total_functions,
    "exported_functions": $exported_count,
    "internal_functions": $internal_count,
    "s3_methods_count": $s3_method_count,
    "s3_classes_count": $s3_class_count,
    "changed_files": $changed_count,
    "cached_files": $cached_count,
    "functions_with_deps": $deps_count
}
EOF

    # Save full dependency map to cache file
    cat > "$CACHE_FILE" << EOF
{
    "metadata": $(cat "$METADATA_FILE"),
    "forward_dependencies": $forward_deps,
    "reverse_dependencies": $reverse_deps
}
EOF

    # Generate markdown visualization
    generate_markdown "$generated_time" "$commit_hash" "$r_files" "$exported_funcs" "$external_deps" "$forward_deps" "$reverse_deps"

    echo "Analysis complete. Files updated:" >&2
    echo "  - $METADATA_FILE" >&2
    echo "  - $VISUALIZATION_FILE" >&2
}

# Generate markdown documentation
generate_markdown() {
    local generated_time="$1"
    local commit_hash="$2"
    local r_files="$3"
    local exported_funcs="$4"
    local external_deps="$5"
    local forward_deps="$6"
    local reverse_deps="$7"

    local pkg_name
    pkg_name=$(get_package_name)
    local pkg_version
    pkg_version=$(get_package_version)

    # Read counts from metadata
    local total_files total_functions exported_count internal_count s3_method_count s3_class_count
    total_files=$(jq -r '.total_files' "$METADATA_FILE")
    total_functions=$(jq -r '.total_functions' "$METADATA_FILE")
    exported_count=$(jq -r '.exported_functions' "$METADATA_FILE")
    internal_count=$(jq -r '.internal_functions' "$METADATA_FILE")
    s3_method_count=$(jq -r '.s3_methods_count' "$METADATA_FILE")
    s3_class_count=$(jq -r '.s3_classes_count' "$METADATA_FILE")

    {
        cat << EOF
# Package Dependency Map

**Generated:** $generated_time
**Package:** $pkg_name v$pkg_version
**Commit:** ${commit_hash:0:7}

## Summary

- **Total Files:** $total_files
- **Total Functions:** $total_functions
- **Exported Functions:** $exported_count
- **Internal Functions:** $internal_count
- **S3 Methods:** $s3_method_count
- **S3 Classes:** $s3_class_count

## External Dependencies
EOF

        # Add external dependencies
        echo "$external_deps" | while read -r dep; do
            [[ -n "$dep" ]] && echo "- $dep"
        done

        cat << 'EOF'

## User Interface (Exported Functions)

### Main Entry Points
EOF

        # Build categorized lists of exported functions
        local core_funcs trend_funcs plot_funcs analysis_funcs
        core_funcs=$(echo "$exported_funcs" | grep -E "^(mvgam|sim_mvgam|series_to_mvgam)$" | tr '\n' ', ' | sed 's/, $//' | sed 's/,$//')
        trend_funcs=$(echo "$exported_funcs" | grep -E "^(RW|AR|VAR|CAR|GP|PW|ZMVN)$" | tr '\n' ', ' | sed 's/, $//' | sed 's/,$//')
        plot_funcs=$(echo "$exported_funcs" | grep "^plot_" | tr '\n' ', ' | sed 's/, $//' | sed 's/,$//')
        analysis_funcs=$(echo "$exported_funcs" | grep -E "^(lfo_cv|loo|ppc|pp_check|conditional_effects|fevd|irf|stability)$" | tr '\n' ', ' | sed 's/, $//' | sed 's/,$//')

        [[ -n "$core_funcs" ]] && echo "- **Core Functions**: $core_funcs"
        [[ -n "$trend_funcs" ]] && echo "- **Trend Types**: $trend_funcs"
        [[ -n "$analysis_funcs" ]] && echo "- **Analysis Tools**: $analysis_funcs"
        [[ -n "$plot_funcs" ]] && echo "- **Plotting Functions**: $plot_funcs"

        cat << 'EOF'

## Core Data Structures
- **mvgam object**: Fitted model with data, parameters, and predictions
- **mvgam_prefit**: Pre-compilation model structure
- **mvgam_forecast**: Forecast results with uncertainty
- **trend_param**: Trend model specifications

## File Organization & Function Locations
EOF

        # Group files by category
        for category in "Core" "Stan/Modeling" "Validation" "S3 Methods" "Plotting" "Utilities" "Data" "Other"; do
            local has_files=false
            local category_content=""

            while IFS= read -r file; do
                [[ -z "$file" ]] && continue
                local file_cat
                file_cat=$(categorize_file "$file")
                if [[ "$file_cat" == "$category" ]]; then
                    local func_count
                    func_count=$(extract_functions_from_file "$file" | wc -l | tr -d ' ')
                    func_count=${func_count:-0}
                    category_content+="- **\`$file\`** ($func_count functions)"$'\n'
                    has_files=true
                fi
            done <<< "$r_files"

            if [[ "$has_files" == "true" ]]; then
                echo ""
                echo "### $category Files"
                echo "$category_content"
            fi
        done

        cat << 'EOF'

## Function Dependencies

### Forward Dependencies (Calls)
EOF

        # Output forward dependencies
        echo "$forward_deps" | jq -r 'to_entries | sort_by(.key) | .[] | "- **\(.key)()** → \(.value)"' 2>/dev/null || echo "No dependencies tracked"

        cat << 'EOF'

### Reverse Dependencies (Called By)
EOF

        # Output reverse dependencies
        echo "$reverse_deps" | jq -r 'to_entries | sort_by(.key) | .[] | "- **\(.key)()** ← \(.value)"' 2>/dev/null || echo "No reverse dependencies tracked"

        cat << 'EOF'

## Function Location Quick Reference

### Exported Functions by File
EOF

        # List exported functions by file
        while IFS= read -r file; do
            [[ -z "$file" ]] && continue
            local file_funcs
            file_funcs=$(extract_functions_from_file "$file")
            local exported_in_file=""

            while IFS= read -r func; do
                [[ -z "$func" ]] && continue
                if echo "$exported_funcs" | grep -qx "$func"; then
                    exported_in_file+="$func, "
                fi
            done <<< "$file_funcs"

            if [[ -n "$exported_in_file" ]]; then
                echo "- **\`$file\`**: ${exported_in_file%, }"
            fi
        done <<< "$r_files"

        cat << 'EOF'

## Notes for LLM Agents
- **Primary workflow**: `mvgam()` -> prior/trend system setup -> Stan assembly -> model fitting -> S3 methods
- **Prior system**: `mvgam_formula()`, `get_prior()`, `set_prior()` for Bayesian prior specification
- **Trend system**: `trend_param()`, `register_trend_type()` for time series trend modeling
- **Stan assembly**: `stancode()`, `standata()` for Stan model compilation and data preparation
- **Stan integration**: Package heavily relies on Stan/brms for Bayesian computation
- **S3 system**: Extensive use of S3 classes (`mvgam`, `mvgam_forecast`, etc.) with method dispatch
- **File patterns**: `R/priors.R` (prior system), `R/trend_system.R` (trends), `R/stan_assembly.R` (Stan code)
- **Core functions often call**: validation functions, Stan code generation, data preprocessing utilities
EOF
    } > "$VISUALIZATION_FILE"
}

# Auto-commit changes to architecture/
auto_commit_changes() {
    if [[ -n "$(git status --porcelain architecture/ 2>/dev/null)" ]]; then
        echo "Auto-committing architecture documentation updates..." >&2
        git add architecture/ 2>/dev/null || true
        git commit --amend --no-edit --quiet 2>/dev/null || \
            git commit -m "Update package dependency documentation" --quiet 2>/dev/null || true
    fi
}

# Main entry point
main() {
    local event="${1:-pre_push}"

    case "$event" in
        pre_push|pre_git_push|test)
            run_analysis
            auto_commit_changes
            ;;
        *)
            # Silent for other events
            ;;
    esac

    exit 0
}

# Run if executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
