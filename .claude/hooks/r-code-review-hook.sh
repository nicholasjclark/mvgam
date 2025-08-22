#!/bin/bash

# R Code Review Hook for Claude Code
# Save this as r-code-review-hook.sh in your Claude Code hooks directory
# Make executable with: chmod +x r-code-review-hook.sh

# Hook configuration
HOOK_NAME="r-code-review-hook"
DESCRIPTION="Reviews R code changes with code-reviewer sub-agent before writing"
CODE_REVIEWER="code-reviewer"

# Function to check if file is an R file
is_r_file() {
    local file_path="$1"
    case "${file_path,,}" in
        *.r|*.rmd|*.rnw) return 0 ;;
        *) return 1 ;;
    esac
}

# Function to extract status from markdown review
extract_review_status() {
    local review_file="$1"
    local status_line=$(grep -m1 "^\*\*Status\*\*:" "$review_file" 2>/dev/null)
    
    if echo "$status_line" | grep -q "✅ APPROVED"; then
        echo "APPROVED"
    elif echo "$status_line" | grep -q "❌ REJECTED"; then
        echo "REJECTED"
    elif echo "$status_line" | grep -q "⚠️ NEEDS REVISION"; then
        echo "NEEDS_REVISION"
    else
        echo "UNKNOWN"
    fi
}

# Function to count high priority issues
count_high_priority_issues() {
    local review_file="$1"
    # Count sections under HIGH PRIORITY that have actual issues
    local high_priority_count=0
    
    # Look for HIGH PRIORITY section and count issues
    if grep -q "## 🚨 HIGH PRIORITY" "$review_file"; then
        # Count lines that start with "- **" under HIGH PRIORITY section
        high_priority_count=$(awk '
            /## 🚨 HIGH PRIORITY/,/## ⚠️ MEDIUM PRIORITY|## 💡 LOW PRIORITY|## ✅ Positive/ {
                if ($0 ~ /^- \*\*.*\*\*:/) count++
            }
            END { print count+0 }
        ' "$review_file")
    fi
    
    echo "$high_priority_count"
}

# Function to display review summary
display_review_summary() {
    local review_file="$1"
    echo ""
    echo "📋 CODE REVIEW SUMMARY"
    echo "======================"
    
    # Extract and display status
    local status=$(extract_review_status "$review_file")
    case "$status" in
        "APPROVED")
            echo "✅ Status: APPROVED"
            ;;
        "REJECTED")
            echo "❌ Status: REJECTED"
            ;;
        "NEEDS_REVISION")
            echo "⚠️  Status: NEEDS REVISION"
            ;;
        *)
            echo "❓ Status: UNKNOWN"
            ;;
    esac
    
    # Show high priority issues count
    local high_priority=$(count_high_priority_issues "$review_file")
    if [ "$high_priority" -gt 0 ]; then
        echo "🚨 High Priority Issues: $high_priority (must be fixed)"
    fi
    
    # Show key sections if they exist
    echo ""
    if grep -q "## 🚨 HIGH PRIORITY" "$review_file"; then
        echo "🚨 HIGH PRIORITY ISSUES FOUND:"
        awk '/## 🚨 HIGH PRIORITY/,/## ⚠️ MEDIUM PRIORITY|## 💡 LOW PRIORITY|## ✅ Positive/ {
            if ($0 ~ /^### ❌/) print "  " $0
            if ($0 ~ /^- \*\*.*\*\*:/) print "    " $0
        }' "$review_file"
        echo ""
    fi
    
    # Show action required section
    if grep -q "## 🎯 Action Required for Approval" "$review_file"; then
        echo "🎯 ACTION REQUIRED:"
        awk '/## 🎯 Action Required for Approval/,/## 📊 Priority Summary|## Architecture Analysis|## Final Recommendation/ {
            if ($0 ~ /^[0-9]+\./) print "  " $0
        }' "$review_file"
        echo ""
    fi
}

# Function to prompt user for decision
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

# Function to review R code
review_r_code() {
    local file_path="$1"
    local content="$2"
    local operation="$3"
    
    # Create temporary files
    local temp_code=$(mktemp)
    local temp_review=$(mktemp)
    
    # Write content to temporary file
    echo "$content" > "$temp_code"
    
    # Create review prompt
    local review_prompt="Please review this R code change:

File: $file_path
Operation: $operation

Please analyze the code in the attached file for:
1. Code quality and R best practices
2. Potential bugs or issues  
3. Performance considerations
4. Documentation completeness
5. Package development standards

Use your standard markdown template format for the review."

    echo "🔍 Sending code to reviewer..."
    
    # Call code-reviewer sub-agent
    if claude-code sub-agent "$CODE_REVIEWER" --input "$temp_code" --prompt "$review_prompt" > "$temp_review" 2>/dev/null; then
        
        # Check if review was successful
        if [ -s "$temp_review" ]; then
            display_review_summary "$temp_review"
            
            local status=$(extract_review_status "$temp_review")
            local high_priority=$(count_high_priority_issues "$temp_review")
            
            # Decision logic based on review
            case "$status" in
                "APPROVED")
                    echo "✅ Code review passed!"
                    cleanup_temp_files "$temp_code" "$temp_review"
                    return 0
                    ;;
                "REJECTED")
                    echo "❌ Code review rejected!"
                    if prompt_user "Proceed anyway?" "n"; then
                        cleanup_temp_files "$temp_code" "$temp_review"
                        return 0
                    else
                        cleanup_temp_files "$temp_code" "$temp_review"
                        return 1
                    fi
                    ;;
                "NEEDS_REVISION")
                    if [ "$high_priority" -gt 0 ]; then
                        echo "⚠️  Code needs revision with $high_priority high priority issues!"
                        if prompt_user "Proceed anyway?" "n"; then
                            cleanup_temp_files "$temp_code" "$temp_review"
                            return 0
                        else
                            cleanup_temp_files "$temp_code" "$temp_review"
                            return 1
                        fi
                    else
                        echo "⚠️  Code needs minor revisions but no critical issues found."
                        if prompt_user "Proceed?" "y"; then
                            cleanup_temp_files "$temp_code" "$temp_review"
                            return 0
                        else
                            cleanup_temp_files "$temp_code" "$temp_review"
                            return 1
                        fi
                    fi
                    ;;
                *)
                    echo "❓ Could not determine review status."
                    if prompt_user "Proceed anyway?" "n"; then
                        cleanup_temp_files "$temp_code" "$temp_review"
                        return 0
                    else
                        cleanup_temp_files "$temp_code" "$temp_review"
                        return 1
                    fi
                    ;;
            esac
        else
            echo "❌ No review response received"
            cleanup_temp_files "$temp_code" "$temp_review"
            return 1
        fi
    else
        echo "❌ Failed to call code reviewer"
        cleanup_temp_files "$temp_code" "$temp_review"
        return 1
    fi
}

# Function to clean up temporary files
cleanup_temp_files() {
    for file in "$@"; do
        [ -f "$file" ] && rm -f "$file"
    done
}

# Main hook function - called by Claude Code
main() {
    local event="$1"
    local file_path="$2"
    local operation="$3"
    
    # Only handle before_write events
    if [ "$event" != "before_write" ]; then
        exit 0
    fi
    
    # Check if this is an R file
    if ! is_r_file "$file_path"; then
        exit 0
    fi
    
    # Skip review for delete operations
    if [ "$operation" = "delete" ]; then
        exit 0
    fi
    
    echo "🔍 Reviewing R code changes for: $file_path"
    
    # Read content from stdin
    local content=$(cat)
    
    # Review the code
    if review_r_code "$file_path" "$content" "$operation"; then
        echo "✅ Proceeding with write operation"
        exit 0
    else
        echo "❌ Aborting write operation"
        exit 1
    fi
}

# Run main function with all arguments
main "$@"
