# Stan Code Generation System - Remaining Tasks

## NON-NEGOTIABLE WORKFLOW
- Proceed with ONE priority task at a time
- Use the pathfinder agent to read `architecture/stan-data-flow-pipeline.md` and to systematically trace the flow from `stancode()` to complete Stan code creation
- The code-reviewer agent MUST be used to approve of any edits BEFORE they are implemented
- Following any edits to R code, agents MUST:
  1. Regenerate currents by running `target_generation.R`
  2. Use parallel general-purpose agents to **READ AND ANALYZE ALL** `current_stancode*` vs `target_stancode*` in the `tasks/` directory
  3. Adhere to a STRICT TDD approach: no fix is verified until ALL currents have been compared to their respective targets

## CRITICAL SUBAGENT INSTRUCTIONS

**SUBAGENTS MUST ONLY READ AND COMPARE FILES - NEVER CREATE OR MODIFY**

When analyzing `current_stancode*` vs `target_stancode*` files:

- **DO**: Use Read tool to examine BOTH current and target files completely
- **DO**: Compare line-by-line differences between current vs target
- **DO**: Identify specific line numbers where differences occur
- **DO**: Report exact code snippets showing current vs target differences
- **DO**: Assess compilation readiness and identify syntax/logic errors

- **DO NOT**: Create new files
- **DO NOT**: Modify existing files  
- **DO NOT**: Write code to disk
- **DO NOT**: Use Write, Edit, or any file modification tools
- **DO NOT**: Attempt to "fix" files directly

**AGENT TASK**: Your ONLY job is to read the existing files and report discrepancies with specific line numbers and code snippets.
  
### Priority 1: Run `target_generation.R`
- [ ] Deploy parallel subagents to systematically compare the created data `.rds` objects against declared data objects in each Stan file
- [ ] Each subagent must provide a comprehensive summary of any discrepancies in data object names, structures and dimensions
- [ ] Summarise subagent findings and present to the user
