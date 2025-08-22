#!/bin/bash

# Duplicate Function Name Detector Hook for R Packages
# Save as: duplicate-function-detector-hook.sh

# Configuration - Set these based on your preferences
SMART_MODE=true              # Only run when new functions are detected
PERIODIC_MODE=true           # Run periodically based on file count
FILES_THRESHOLD=10           # Run every N file changes (if PERIODIC_MODE=true)
MANUAL_TRIGGER_FILE=".claude/run-duplicate-check"  # Touch this file to force a run

# Counter file for periodic mode
COUNTER_FILE=".claude/duplicate-check-counter"

# Main hook function
main() {
    local event="$1"
    local file_path="$2"
    local operation="$3"
    
    # Only handle before_write events for R files
    if [ "$event" != "before_write" ] || ! is_r_file "$file_path"; then
        exit 0
    fi
    
    # Skip for delete operations
    if [ "$operation" = "delete" ]; then
        exit 0
    fi
    
    # Read new content from stdin
    local new_content=$(cat)
    
    # Determine if we should run the check
    if ! should_run_check "$file_path" "$new_content" "$operation"; then
        echo "⏭️  Skipping duplicate function check (no new functions detected)"
        exit 0
    fi
    
    echo "🔍 Checking for duplicate function names in R package..."
    
    # Check for duplicates
    if check_duplicate_functions "$file_path" "$new_content" "$operation"; then
        echo "✅ No duplicate functions detected"
        exit 0
    else
        echo "❌ Duplicate functions found - aborting write operation"
        exit 1
    fi
}

# Determine whether to run the duplicate check
should_run_check() {
    local file_path="$1"
    local new_content="$2"
    local operation="$3"
    
    # Always run if manual trigger file exists
    if [ -f "$MANUAL_TRIGGER_FILE" ]; then
        echo "🔄 Manual trigger detected"
        rm -f "$MANUAL_TRIGGER_FILE"
        return 0
    fi
    
    # Smart mode: only run when new functions are detected
    if [ "$SMART_MODE" = true ]; then
        if has_new_functions "$file_path" "$new_content" "$operation"; then
            echo "📝 New function(s) detected, running duplicate check"
            return 0
        else
            return 1
        fi
    fi
    
    # Periodic mode: run every N file changes
    if [ "$PERIODIC_MODE" = true ]; then
        local count=$(get_file_change_count)
        if [ "$count" -ge "$FILES_THRESHOLD" ]; then
            echo "📊 Periodic check triggered (${count} file changes)"
            reset_file_change_count
            return 0
        else
            increment_file_change_count
            echo "📈 File change count: $count/$FILES_THRESHOLD"
            return 1
        fi
    fi
    
    # Default: always run (original behavior)
    return 0
}

# Check if new functions are being added
has_new_functions() {
    local file_path="$1"
    local new_content="$2"
    local operation="$3"
    
    # Extract function names from new content
    local new_functions=$(extract_function_names "$new_content")
    
    if [ -z "$new_functions" ]; then
        return 1  # No functions in new content
    fi
    
    # For create operations, any function is "new"
    if [ "$operation" = "create" ] || [ ! -f "$file_path" ]; then
        return 0
    fi
    
    # For modify operations, compare with existing content
    if [ -f "$file_path" ]; then
        local existing_functions=$(extract_function_names "$(cat "$file_path")")
        
        # Check if any function in new content is not in existing content
        while IFS= read -r func_name; do
            if [ -n "$func_name" ] && ! echo "$existing_functions" | grep -q "^$func_name$"; then
                echo "🆕 New function detected: $func_name"
                return 0
            fi
        done <<< "$new_functions"
        
        # Also check for significant changes to existing functions
        local modified_functions=""
        while IFS= read -r func_name; do
            if [ -n "$func_name" ] && echo "$existing_functions" | grep -q "^$func_name$"; then
                # Check if function signature changed significantly
                local old_sig=$(extract_function_signature "$func_name" "$file_path")
                local new_sig=$(echo "$new_content" | extract_function_signature_from_content "$func_name")
                
                if [ -n "$old_sig" ] && [ -n "$new_sig" ] && [ "$old_sig" != "$new_sig" ]; then
                    local param_diff=$(compare_function_parameters "$old_sig" "$new_sig")
                    if [ "$param_diff" -gt 2 ]; then  # Significant change threshold
                        echo "🔄 Significant change to function: $func_name"
                        return 0
                    fi
                fi
            fi
        done <<< "$new_functions"
    fi
    
    return 1  # No new functions detected
}

# Compare function parameters (simple difference count)
compare_function_parameters() {
    local old_sig="$1"
    local new_sig="$2"
    
    # Extract parameter lists
    local old_params=$(echo "$old_sig" | sed -E 's/.*function[[:space:]]*\(([^)]*)\).*/\1/' | tr ',' '\n' | wc -l)
    local new_params=$(echo "$new_sig" | sed -E 's/.*function[[:space:]]*\(([^)]*)\).*/\1/' | tr ',' '\n' | wc -l)
    
    echo $((new_params > old_params ? new_params - old_params : old_params - new_params))
}

# Extract function signature from content string
extract_function_signature_from_content() {
    local func_name="$1"
    
    awk -v func="$func_name" '
    $0 ~ "^[[:space:]]*" func "[[:space:]]*<-[[:space:]]*function[[:space:]]*\\(" {
        signature = $0
        if ($0 ~ /function[[:space:]]*\([[:space:]]*$/) {
            while (getline && $0 !~ /\)[[:space:]]*\{?[[:space:]]*$/) {
                signature = signature " " $0
            }
            signature = signature " " $0
        }
        gsub(/^[[:space:]]*/, "", signature)
        gsub(/[[:space:]]*\{[[:space:]]*$/, "", signature)
        print signature
        exit
    }'
}

# Periodic mode helper functions
get_file_change_count() {
    if [ -f "$COUNTER_FILE" ]; then
        cat "$COUNTER_FILE"
    else
        echo 0
    fi
}

increment_file_change_count() {
    local count=$(get_file_change_count)
    echo $((count + 1)) > "$COUNTER_FILE"
}

reset_file_change_count() {
    echo 0 > "$COUNTER_FILE"
}

# Function to check if file is an R file
is_r_file() {
    local file_path="$1"
    case "${file_path,,}" in
        *.r|*.rmd|*.rnw) return 0 ;;
        *) return 1 ;;
    esac
}

# Main duplicate checking function
check_duplicate_functions() {
    local target_file="$1"
    local new_content="$2"
    local operation="$3"
    
    # Extract function names from new content
    local new_functions=$(extract_function_names "$new_content")
    
    if [ -z "$new_functions" ]; then
        return 0  # No functions to check
    fi
    
    echo "📋 Functions in new/modified content:"
    echo "$new_functions" | sed 's/^/  - /'
    
    # Find all existing R files in the package
    local r_files=$(find_r_files_in_package)
    
    # Check for duplicates
    local duplicates_found=""
    local potential_conflicts=""
    
    while IFS= read -r func_name; do
        if [ -n "$func_name" ]; then
            # Check each existing R file
            while IFS= read -r existing_file; do
                if [ -f "$existing_file" ] && [ "$existing_file" != "$target_file" ]; then
                    local existing_functions=$(extract_function_names "$(cat "$existing_file")")
                    
                    if echo "$existing_functions" | grep -q "^$func_name$"; then
                        duplicates_found="$duplicates_found\n  🔴 '$func_name' already exists in: $existing_file"
                    fi
                fi
            done <<< "$r_files"
            
            # For modify operations, also check if function already exists in the same file
            if [ "$operation" = "modify" ] && [ -f "$target_file" ]; then
                local current_functions=$(extract_function_names "$(cat "$target_file")")
                if echo "$current_functions" | grep -q "^$func_name$"; then
                    potential_conflicts="$potential_conflicts\n  🟡 '$func_name' being redefined in same file: $target_file"
                fi
            fi
        fi
    done <<< "$new_functions"
    
    # Report findings
    local has_issues=false
    
    if [ -n "$duplicates_found" ]; then
        echo ""
        echo "❌ DUPLICATE FUNCTIONS DETECTED:"
        echo -e "$duplicates_found"
        has_issues=true
    fi
    
    if [ -n "$potential_conflicts" ]; then
        echo ""
        echo "⚠️  POTENTIAL REDEFINITIONS:"
        echo -e "$potential_conflicts"
    fi
    
    if [ "$has_issues" = true ]; then
        echo ""
        echo "🔧 RESOLUTION OPTIONS:"
        echo "  1. Rename the function(s) to be unique"
        echo "  2. Remove duplicate implementation"
        echo "  3. Merge implementations if appropriate"
        echo "  4. Move to existing file if it's the same function"
        
        # Show function signatures for comparison
        show_function_signatures "$new_functions" "$r_files"
        
        echo ""
        if prompt_user "Do you want to proceed anyway? (This will create duplicate functions)" "n"; then
            echo "⚠️  Proceeding with duplicate functions - consider resolving later"
            return 0
        else
            echo "❌ Aborting to avoid duplicate functions"
            return 1
        fi
    fi
    
    return 0
}

# Extract function names from R content
extract_function_names() {
    local content="$1"
    
    # Extract function definitions using various patterns
    echo "$content" | awk '
    # Standard function definition: name <- function(...)
    /^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_.]*[[:space:]]*<-[[:space:]]*function[[:space:]]*\(/ {
        match($0, /^[[:space:]]*([a-zA-Z_][a-zA-Z0-9_.]*)[[:space:]]*<-/, arr)
        if (arr[1]) print arr[1]
    }
    
    # Alternative assignment: name = function(...)
    /^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_.]*[[:space:]]*=[[:space:]]*function[[:space:]]*\(/ {
        match($0, /^[[:space:]]*([a-zA-Z_][a-zA-Z0-9_.]*)[[:space:]]*=/, arr)
        if (arr[1]) print arr[1]
    }
    
    # Function assignment with quotes: "name" <- function(...)
    /^[[:space:]]*["\x27][a-zA-Z_][a-zA-Z0-9_.]*["\x27][[:space:]]*<-[[:space:]]*function[[:space:]]*\(/ {
        match($0, /^[[:space:]]*["\x27]([a-zA-Z_][a-zA-Z0-9_.]*)["\x27][[:space:]]*<-/, arr)
        if (arr[1]) print arr[1]
    }
    ' | sort -u
}

# Find all R files in the package
find_r_files_in_package() {
    # Look for R files in standard package locations
    find . -type f \( -name "*.R" -o -name "*.r" \) \
        -path "./R/*" \
        -o -path "./tests/*" \
        -o -path "./inst/*" \
        2>/dev/null | \
    grep -v "./.claude/" | \
    sort
}

# Show function signatures for comparison
show_function_signatures() {
    local new_functions="$1"
    local r_files="$2"
    
    echo ""
    echo "🔍 FUNCTION SIGNATURES COMPARISON:"
    
    while IFS= read -r func_name; do
        if [ -n "$func_name" ]; then
            echo ""
            echo "📋 Function: $func_name"
            
            # Find existing implementations
            while IFS= read -r existing_file; do
                if [ -f "$existing_file" ]; then
                    local signature=$(extract_function_signature "$func_name" "$existing_file")
                    if [ -n "$signature" ]; then
                        echo "  📁 $existing_file:"
                        echo "     $signature"
                    fi
                fi
            done <<< "$r_files"
        fi
    done <<< "$new_functions"
}

# Extract function signature from file
extract_function_signature() {
    local func_name="$1"
    local file_path="$2"
    
    # Extract the function definition line(s)
    awk -v func="$func_name" '
    $0 ~ "^[[:space:]]*" func "[[:space:]]*<-[[:space:]]*function[[:space:]]*\\(" {
        # Found function start, capture the signature
        signature = $0
        
        # If the line ends with just "function(" we need to get the parameters
        if ($0 ~ /function[[:space:]]*\([[:space:]]*$/) {
            # Parameters are on next lines
            while (getline && $0 !~ /\)[[:space:]]*\{?[[:space:]]*$/) {
                signature = signature " " $0
            }
            signature = signature " " $0
        }
        
        # Clean up the signature
        gsub(/^[[:space:]]*/, "", signature)
        gsub(/[[:space:]]*\{[[:space:]]*$/, "", signature)
        print signature
        exit
    }
    ' "$file_path"
}

# Utility function to prompt user
prompt_user() {
    local message="$1"
    local default="$2"
    
    echo -n "$message "
    if [ "$default" = "n" ]; then
        echo -n "(y/N): "
    else
        echo -n "(Y/n): "
    fi
    
    read -r response
    case "$response" in
        [Yy]|[Yy][Ee][Ss]) return 0 ;;
        [Nn]|[Nn][Oo]) return 1 ;;
        "") [ "$default" = "y" ] && return 0 || return 1 ;;
        *) return 1 ;;
    esac
}

# Run main function with all arguments
main "$@"
