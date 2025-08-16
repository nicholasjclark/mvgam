# R Package Task List Management

Guidelines for managing task lists in markdown files to track progress on completing Task Requirements Documents (TRDs) for R package development.

## Task Implementation Protocol

### Sub-Task Execution
- **MANDATORY STOP:** After completing each sub-task, you **MUST** stop and ask user permission before proceeding
- **One sub-task at a time:** Do **NOT** start the next sub‑task until you ask the user for permission and they say "yes" or "y"
- **15-minute rule:** Each sub-task must be completable within 15 minutes
- **Code review:** ALWAYS use the code-reviewer agent to verify any proposed R code changes
- **Mark completed:** Change `[ ]` to `[x]` immediately upon sub-task completion
- **Always pause:** Never proceed to the next sub-task without explicit user approval

### Parent Task Completion Sequence
When **all** subtasks under a parent task are `[x]`:

1. **Test & Validate:**
   - Run `devtools::load_all(); devtools::test()` - all tests must pass

2. **Stage & Clean:**
   - Stage changes: `git add .`
   - Remove temporary files and debug code
   - Ensure roxygen2 documentation is current

3. **Commit:**
   ```bash
   git commit -m "feat: [parent task summary]" \
     -m "- [key function/method added]" \
     -m "- [key changes made]" \
     -m "Related to T[X.0] in [TRD-name]"
   ```

4. **Mark parent task `[x]`**

### Full Task List Completion
When **ALL** tasks are `[x]`:

1. **Final Validation:**
   - `devtools::load_all()` (loads cleanly)
   - `devtools::test()` (all pass)
   - All documentation examples run successfully

3. **Final Code Review:**
   - Submit complete implementation to `code-reviewer` agent (check the actual file contents first using Read tool before sending to code-reviewer)
   - Address high-priority feedback
   - Update `active/code_improvements.md` with medium/low priority items to ensure future agents can learn from any failures

4. **Final Commit & Tag:**
   ```bash
   git commit -m "feat: complete [TRD-name] implementation" \
     -m "- [summary of major functions added]" \
     -m "- [key capabilities delivered]" \
     -m "- Code reviewer approved: [date]"
   git tag -a v[version]-[feature-name] -m "Complete [TRD-name]"
   ```

5. **Generate Completion Summary:**
   - Feature overview and functions added
   - Files created/modified inventory
   - Testing coverage summary
   - Outstanding items reference
   - Save as `deliverables/[TRD-name]-completion-summary.md`

6. **Archive & Clean:**
   - Move task list to `completed-tasks/` directory
   - Update `active/architecture-decisions.md` if necessary
   - Remove temporary files
   - Notify user of completion

## AI Instructions

### Required Actions (NON-NEGOTIABLE)
1. **Code Review:** Use `code-reviewer` agent for ALL R code changes
2. **File Management:** 
   - Read/create `active/code_improvements.md`
   - Check `active/` directory for context files
   - Keep `active/architecture-decisions.md` under 600 lines

3. **Standards Compliance:**
   - Follow existing package naming conventions
   - Ensure roxygen2 documentation for all functions (`@param`, `@return`, `@examples`, `@export`)
   - Write comprehensive tests in `tests/testthat/`
   - Implement proper input validation

### Workflow Process
1. Check which sub-task is next
2. Present planned code to code reviewer (before implementation)
3. Implement sub-task following R package best practices
4. Submit completed code for final review
5. Address high-priority feedback immediately
6. Summarize medium/low priority items in `active/code_improvements.md` for future agents to learn from
7. **STOP AND WAIT:** Update task list, mark sub-task `[x]`, and ask user "Ready for the next sub-task?" - Wait for "yes" or "y" before proceeding

### Task List Maintenance
- Mark completed tasks `[x]` per protocol
- Add newly discovered tasks as they emerge
- Update "Relevant Files" section with all R files in `R/`, tests in `tests/testthat/`, and package-level changes
- Give each file a one-line purpose description

### Quality Gates
Before marking any parent task complete:
- [ ] All tests pass (`devtools::load_all(); devtools::test()`)
- [ ] Code reviewer approval received
- [ ] Documentation complete with working examples
- [ ] No temporary or debug code remains

## Commit Message Examples

```bash
# Function implementation
git commit -m "feat: implement data validation functions" \
  -m "- Add validate_input() with comprehensive checks" \
  -m "- Include S3 methods for different data types" \
  -m "Related to T1.2 in TRD-data-processing"

# Testing
git commit -m "test: add validation test suite" \
  -m "- Tests edge cases and error conditions" \
  -m "- Validates S3 method dispatch" \
  -m "Related to T3.1 in TRD-data-processing"

# Documentation  
git commit -m "docs: add roxygen2 documentation" \
  -m "- Complete @param and @return docs" \
  -m "- Add runnable examples" \
  -m "Related to T4.1 in TRD-data-processing"
```
