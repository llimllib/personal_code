---
name: pr-transcript
description: Export the current Pi session transcript to an HTML file for inclusion in a pull request. Use when the user is ready to submit a PR and wants to include an AI session transcript.
---

# PR Transcript Export

This skill exports the current Pi session to an HTML file for PR documentation.

## Quick Usage

Use the helper script (relative to this skill directory):

```bash
./export-transcript.sh <pr-number> <description> [output-dir]
```

Example:
```bash
./.pi/skills/pr-transcript/export-transcript.sh 21 replace-release-with-goreleaser
# Exports to: transcripts/21-replace-release-with-goreleaser.html
```

## Manual Process

If the script doesn't work or you need more control:

1. **Find the current session file**:
   ```bash
   # Session files are stored based on the working directory
   SESSION_DIR="$HOME/.pi/agent/sessions/--$(pwd | tr '/' '-' | sed 's/^-//')--"
   
   # List sessions to find the current one (most recent)
   ls -t "$SESSION_DIR"/*.jsonl | head -1
   ```

2. **Export the transcript**:
   ```bash
   pi --export <session_file> transcripts/<pr_number>-<description>.html
   ```

## Workflow

When the user says they're ready to submit a PR or asks for a transcript:

1. Find the most recent session file for the current directory
2. Ask for PR number and a short description (if not provided)
3. Create the transcripts directory if it doesn't exist
4. Run `pi --export` with the session file and output path
5. Confirm the file was created
6. Suggest adding the transcript to the PR description

## Example

User: "I'm ready to submit a PR, please export the transcript"

1. Find session: `~/.pi/agent/sessions/--Users-llimllib-code-mdriver-goreleaser--/2026-01-16T15-41-47-921Z_abc123.jsonl`
2. Ask: "What's the PR number and a short description?"
3. User: "PR 21, replace-release-with-goreleaser"
4. Run: `pi --export <session> transcripts/21-replace-release-with-goreleaser.html`
5. Output: "Transcript exported to transcripts/21-replace-release-with-goreleaser.html"

## Notes

- The session directory is named after the current working directory with slashes replaced by dashes
- Multiple session files may exist; use the most recent one (sorted by timestamp in filename)
- Create `transcripts/` directory if it doesn't exist
- The description should be kebab-case (lowercase with hyphens)
