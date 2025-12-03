---
description: Commit and push all current changes
allowed-tools: Bash(git checkout --branch:*), Bash(git add:*), Bash(git status:*), Bash(git push:*), Bash(git commit:*), Bash(gh pr create:*)
model: haiku 4.5
---

- Commit and push ALL changes in the project, following the explicit guidelines in CLAUDE.md. 
- Ignore any metadata updates to architecture/ that may be added by the git hook
- You have the capability to call multiple tools in a single response. You MUST do all of the above in a single message. Do not use any other tools or do anything else. 
