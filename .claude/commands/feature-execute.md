# R Package Task List Management

Guidelines for managing task lists in markdown files to track progress on completing a Task Requirements Document (TRD) for R package development.

## Task Implementation

- **One sub-task at a time:** Do **NOT** start the next sub‑task until you ask the user for permission and they say "yes" or "y"
- **15-minute rule:** Each sub-task should be completable within 15 minutes. If a task is taking longer, break it down further or seek guidance.
- **Completion protocol:**     
  1. When you finish a **sub‑task**, immediately mark it as completed by changing `[ ]` to `[x]`.
  2. If **all** subtasks underneath a parent task are now `[x]`, follow this sequence:
    - **First**: Run R package tests using `devtools::test(path/to/file)`
    - **Second**: Run package check using `devtools::check()` to ensure R package compliance
    - **Only if all tests and checks pass**: Stage changes (`git add .`)
    - **Clean up**: Remove any temporary files, debug code, and ensure roxygen2 documentation is current
    - **Commit**: Use a descriptive commit message that:
      - Uses conventional commit format (`feat:`, `fix:`, `refactor:`, `test:`, `docs:`, etc.)
      - Summarizes what was accomplished in the parent task
      - Lists key R functions/methods added or modified
      - References the task number and TRD context
      - **Formats the message as a single-line command using `-m` flags**, e.g.:

        ```
        git commit -m "feat: add model fitting functions" -m "- Implements fit_model() with S3 methods" -m "- Adds input validation and error handling" -m "- Includes comprehensive test coverage" -m "Related to T2.0 in TRD-model-fitting"
        ```
  3. Once all the subtasks are marked completed and changes have been committed, mark the **parent task** as completed.
- Stop after each sub‑task and wait for the user's go‑ahead.

## R Package Development Standards

### Code Quality Requirements
- Follow existing package naming conventions and patterns
- Ensure all functions have proper roxygen2 documentation
- Implement appropriate input validation for all user-facing functions
- Follow R package best practices (avoid global assignments, handle missing values appropriately)
- Ensure S3 methods follow proper conventions if applicable

### Testing Requirements
- Write comprehensive tests in `tests/testthat/` directory
- Test edge cases and error conditions
- Ensure test coverage for all exported functions
- Include examples that demonstrate typical usage patterns

### Documentation Requirements
- Use roxygen2 for all function documentation
- Include `@param`, `@return`, `@examples`, and `@export` tags as appropriate
- Update package-level documentation if adding new functionality
- Ensure examples in documentation are runnable

## Task List Maintenance

1. **Update the task list as you work:**
   - Mark tasks and subtasks as completed (`[x]`) per the protocol above.
   - Add new tasks as they emerge during development.
   - Update time estimates if tasks consistently take longer than 15 minutes.

2. **Maintain the "Relevant Files" section:**
   - List every R file created or modified in `R/` directory.
   - List corresponding test files in `tests/testthat/`.
   - Include any updated documentation files.
   - Give each file a one‑line description of its purpose.
   - Note any changes to `DESCRIPTION`, `NAMESPACE`, or other package-level files.

## AI Instructions

When working with R package task lists, the AI must:

1. **File Management:**
   - Read `active/code_improvements.md` if it exists. If it does not exist, create it and leave blank for now.
   - Check `active/` directory for any relevant context files before starting work.
   - Keep `active/architecture-decisions.md` accurate, up to date, and less than 600 lines total.

2. **Code Review Integration (NON-NEGOTIABLE):**
   - You **MUST** use the `code-reviewer` agent to review any proposed changes to R code.
   - Submit R functions, methods, and significant code changes for review before implementation.
   - You **MUST** ensure any medium and low priority items identified by the code reviewer are represented in `active/code_improvements.md`.
   - Address high-priority feedback from code reviewer before proceeding to next sub-task.
   - Document reviewer feedback and resolutions in task comments.

3. **Implementation Standards:**
   - Regularly use thinking to ensure implementations guarantee extendability, modularity, and code simplicity.
   - Follow R package conventions (snake_case for functions, proper S3 dispatch, etc.).
   - Ensure all code integrates properly with existing package architecture.
   - Validate that new functions work with expected R workflows (base R and tidyverse patterns).

4. **Progress Management:**
   - Before starting work, check which sub‑task is next in the task list.
   - After implementing a sub‑task, update the task file and then pause for user approval.
   - Regularly update the task list file after finishing any significant work.
   - Add newly discovered tasks or sub-tasks as they become apparent.

5. **R Package Validation:**
   - Run `devtools::load_all()` to ensure package loads correctly after changes.
   - Use `devtools::document()` to update documentation after roxygen2 changes.
   - Check that `NAMESPACE` exports are correct for new functions.
   - Verify that examples in documentation run without errors.

## Code Reviewer Integration Workflow

1. **Before Implementation:** Present planned R code structure to code reviewer
2. **During Implementation:** Submit complex functions or algorithms for mid-development review
3. **After Implementation:** Submit completed code for final review before marking sub-task complete
4. **Feedback Processing:** 
   - Address high-priority issues immediately
   - Log medium/low priority improvements in `active/code_improvements.md`
   - Document any architectural decisions in `active/architecture-decisions.md`

## R-Specific Commit Message Examples

```bash
# Function implementation
git commit -m "feat: implement data validation functions" -m "- Add validate_input() with comprehensive checks" -m "- Include S3 methods for different data types" -m "Related to T1.2 in TRD-data-processing"

# Testing
git commit -m "test: add comprehensive test suite for validation" -m "- Tests for edge cases and error conditions" -m "- Validates S3 method dispatch" -m "Related to T3.1 in TRD-data-processing"

# Documentation
git commit -m "docs: add roxygen2 documentation for validation functions" -m "- Complete @param and @return documentation" -m "- Add runnable examples" -m "Related to T4.1 in TRD-data-processing"

# Bug fixes
git commit -m "fix: handle missing values in validation logic" -m "- Add explicit NA handling" -m "- Update tests for missing data scenarios" -m "Addresses code reviewer feedback T1.2"
```

## Quality Gates

Before marking any parent task as complete:
- [ ] All tests in `tests/testthat/` pass
- [ ] Code reviewer has approved all changes
- [ ] Documentation is complete and examples run successfully
- [ ] Functions follow existing package conventions
- [ ] No temporary or debug code remains
