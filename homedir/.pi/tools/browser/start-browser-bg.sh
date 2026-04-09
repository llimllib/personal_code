#!/bin/bash
# Wrapper script to start browser in background using nohup

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="/tmp/browser-start.log"

# Parse args to check for --force flag
FORCE=false
for arg in "$@"; do
  if [ "$arg" = "--force" ]; then
    FORCE=true
    break
  fi
done

# Check if browser is already running
if [ -f /tmp/browser-endpoint.txt ]; then
  # Try to connect to see if it's really running
  if "$SCRIPT_DIR/eval-js.js" "1+1" >/dev/null 2>&1; then
    if [ "$FORCE" = false ]; then
      echo "✓ Browser already running"
      cat /tmp/browser-endpoint.txt | sed 's/^/  WebSocket: /'
      echo "  Endpoint file: /tmp/browser-endpoint.txt"
      echo ""
      echo "Use --force to kill and restart, or use other tools to interact with it."
      exit 0
    else
      echo "Killing existing browser..."
      pkill -f ".pi/chrome-profile" 2>/dev/null || true
      sleep 1
      rm -f /tmp/browser-endpoint.txt
      echo "✓ Killed existing browser"
    fi
  else
    # Stale endpoint file
    rm -f /tmp/browser-endpoint.txt
  fi
fi

# Filter out --force and --detach from args for the Node script
ARGS=()
for arg in "$@"; do
  if [ "$arg" != "--force" ] && [ "$arg" != "--detach" ]; then
    ARGS+=("$arg")
  fi
done

# Start browser in background with nohup
echo "Starting Chromium in background..."
nohup node "$SCRIPT_DIR/start-browser-daemon.js" "${ARGS[@]}" > "$LOG_FILE" 2>&1 &

# Wait a moment for it to start
sleep 2

# Check if it started successfully
if [ -f /tmp/browser-endpoint.txt ]; then
  echo "✓ Browser started"
  cat /tmp/browser-endpoint.txt | sed 's/^/  WebSocket: /'
  echo "  Endpoint file: /tmp/browser-endpoint.txt"
  echo "  Log file: $LOG_FILE"
  echo ""
  echo "Browser running in background. Close browser window to exit."
else
  echo "✗ Failed to start browser. Check $LOG_FILE for details."
  exit 1
fi
