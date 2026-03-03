#!/usr/bin/env bash
# Export the most recent Pi session transcript for the current directory
# Usage: export-transcript.sh <pr-number> <description> [output-dir]

set -euo pipefail

PR_NUMBER="${1:-}"
DESCRIPTION="${2:-}"
OUTPUT_DIR="${3:-transcripts}"

if [[ -z "$PR_NUMBER" || -z "$DESCRIPTION" ]]; then
    echo "Usage: $0 <pr-number> <description> [output-dir]"
    echo "Example: $0 21 replace-release-with-goreleaser"
    exit 1
fi

# Convert description to kebab-case
DESCRIPTION=$(echo "$DESCRIPTION" | tr '[:upper:]' '[:lower:]' | tr ' ' '-' | tr -cd 'a-z0-9-')

# Build session directory path from current working directory
SESSION_DIR_NAME="--$(pwd | tr '/' '-' | sed 's/^-//')--"
SESSION_DIR="$HOME/.pi/agent/sessions/$SESSION_DIR_NAME"

if [[ ! -d "$SESSION_DIR" ]]; then
    echo "Error: Session directory not found: $SESSION_DIR"
    exit 1
fi

# Find the most recent session file
SESSION_FILE=$(find "$SESSION_DIR" -maxdepth 1 -name "*.jsonl" -print0 2>/dev/null | xargs -0 ls -t 2>/dev/null | head -1)

if [[ -z "$SESSION_FILE" ]]; then
    echo "Error: No session files found in $SESSION_DIR"
    exit 1
fi

echo "Found session: $SESSION_FILE"

# Create output directory if needed
mkdir -p "$OUTPUT_DIR"

# Build output filename
OUTPUT_FILE="$OUTPUT_DIR/${PR_NUMBER}-${DESCRIPTION}.html"

# Export the transcript
echo "Exporting to: $OUTPUT_FILE"
pi --export "$SESSION_FILE" "$OUTPUT_FILE"

echo "Done! Transcript exported to: $OUTPUT_FILE"
