# PR Body Generator Template

You are helping create a PR body for the posit-dev/positron repository. Follow these guidelines:

## Context
 
You MUST use your github tool to look up the corresponding issue #$ARGUMENTS that this PR is addressing. Ask questions to clarify any unknowns.

## Structure

1. **Opening Line**: Start with "Addresses #[issue_number]." (note the period)

2. **Description**: 
   - 2-4 sentences explaining what the PR does
   - Be direct and technical - assume readers understand the codebase
   - Mention if this PR is paired with other PRs in related repos
   - Include any important technical context

3. **Screenshots**: If UI changes, add placeholder: `[Screenshot: Description of what it shows]`

4. **Release Notes**:
   - Only fill in sections that apply (New Features OR Bug Fixes)
   - Use brief, user-facing language
   - Delete the "N/A" for sections you fill in
   - Keep the other section with "N/A"

5. **QA Notes**:
   - Always include specific, runnable code examples
   - Use triple backticks with language identifier (```python, ```r, etc.)
   - Describe expected behavior after running the code
   - Include any special setup steps if needed

## Style Guidelines
- Technical but concise
- No flowery language or unnecessary context
- Focus on what changed and how to verify it
- Use present tense for descriptions ("enables", "fixes", "adds")

## Example Pattern:
```
Addresses #[issue].

[What the PR does in 1-2 sentences]. [Any additional technical context or related PRs].

### Release Notes

#### New Features
- [User-facing description of new functionality]

#### Bug Fixes
- N/A

### QA Notes

[Brief instruction]. [Expected outcome].

```[language]
[Runnable code example]
```
```

## When asking for PR info, start with:
"What issue number does this PR address, and what's the main problem it's solving?"

Then follow up with:
- "Are there any UI changes that need screenshots?"
- "Is this paired with PRs in other repos?"
- "What's the best way to test this change?"

