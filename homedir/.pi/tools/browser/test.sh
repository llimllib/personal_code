#!/bin/bash
# Simple test script for browser tools

echo "Testing browser tools..."
echo ""

# Check if Chromium exists
if [ ! -f "/Applications/Chromium.app/Contents/MacOS/Chromium" ]; then
  echo "❌ Chromium not found at /Applications/Chromium.app"
  exit 1
fi
echo "✓ Chromium found"

# Check if scripts are executable
for script in start-browser.js navigate.js eval-js.js screenshot.js get-cookies.js set-cookies.js; do
  if [ ! -x "$script" ]; then
    echo "❌ $script is not executable"
    exit 1
  fi
done
echo "✓ All scripts are executable"

# Check if node_modules exists
if [ ! -d "node_modules" ]; then
  echo "❌ node_modules not found. Run: npm install"
  exit 1
fi
echo "✓ node_modules installed"

echo ""
echo "All checks passed! ✓"
echo ""
echo "To test manually:"
echo "  1. ./start-browser.js"
echo "  2. In another terminal:"
echo "     ./navigate.js https://example.com"
echo "     ./eval-js.js \"document.title\""
echo "     ./screenshot.js /tmp/test.png"
