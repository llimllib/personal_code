# Browser Tools for Jellyfish Development

Simple CLI tools for browser automation using Chromium. These tools allow the agent to debug the Jellyfish web application during development.

## Installation

```bash
cd .pi/tools/browser
npm install
chmod +x *.js
```

## Core Tools

### start-browser.js

Start Chromium with persistent profile (for SSO authentication).

```bash
./start-browser.js                    # Start with persistent profile
./start-browser.js --fresh            # Start with temporary profile
./start-browser.js --url <url>        # Start and navigate to URL
```

The browser stays open and other tools connect to it via WebSocket. Press Ctrl+C to close.

**Profile location:** `~/.pi/chrome-profile`  
**WebSocket endpoint:** Saved to `/tmp/browser-endpoint.txt`

### navigate.js

Navigate to a URL.

```bash
./navigate.js http://localhost:3000
./navigate.js http://localhost:3000/issues --new-tab
```

### eval-js.js

Execute JavaScript in the current page.

```bash
./eval-js.js "document.title"
./eval-js.js "document.querySelectorAll('.issue').length"
./eval-js.js "window.location.href"
./eval-js.js --file test-script.js
```

Results are printed as JSON.

### screenshot.js

Take a screenshot of the current page.

```bash
./screenshot.js                       # Saves to /tmp/screenshot-<timestamp>.png
./screenshot.js ~/Desktop/page.png    # Saves to specific path
./screenshot.js --fullpage            # Full page screenshot
```

### get-console.js

Capture console messages from the browser.

```bash
./get-console.js                # Show info about console access
./get-console.js --follow       # Follow console output in real-time
```

**Note:** Cannot read past console messages. Use `--follow` mode before triggering actions you want to debug.

### get-cookies.js

Export cookies (for saving authentication state).

```bash
./get-cookies.js                           # Print to stdout
./get-cookies.js ~/.pi/jellyfish-auth.json # Save to file
```

### set-cookies.js

Import cookies (for restoring authentication state).

```bash
./set-cookies.js ~/.pi/jellyfish-auth.json
```

## SSO Authentication Workflow

For the Jellyfish app secured with SSO:

1. **First time setup:**
   ```bash
   # Start browser with persistent profile
   ./start-browser.js
   
   # Navigate to your app
   ./navigate.js http://localhost:3000
   
   # Log in manually in the browser window
   # (complete SSO flow)
   
   # Save cookies for backup
   ./get-cookies.js ~/.pi/jellyfish-auth.json
   ```

2. **Future sessions:**
   ```bash
   # Start browser - you're already logged in!
   ./start-browser.js
   
   # Navigate to your app
   ./navigate.js http://localhost:3000
   
   # Agent can now interact with authenticated app
   ```

The persistent profile (`~/.pi/chrome-profile`) remembers your authentication state across sessions.

## Agent Usage Examples

Once the browser is started and authenticated, the agent can:

**Check page title:**
```bash
./eval-js.js "document.title"
```

**Count elements:**
```bash
./eval-js.js "document.querySelectorAll('.issue-card').length"
```

**Get text content:**
```bash
./eval-js.js "document.querySelector('.error-message')?.textContent"
```

**Check for errors:**
```bash
./eval-js.js "console.error.toString()"
```

**Take screenshot for visual debugging:**
```bash
./screenshot.js /tmp/current-state.png
```

**Test form submission:**
```bash
./eval-js.js "document.querySelector('form').submit(); 'submitted'"
```

## Troubleshooting

**"Browser not running"**
- Start the browser first: `./start-browser.js`

**"Could not find Chromium"**
- Check that Chromium is installed at: `/Applications/Chromium.app`
- Update `CHROMIUM_PATH` in `start-browser.js` if needed

**"Navigation timeout"**
- Your dev server might not be running
- Check URL is correct
- Try increasing timeout in navigate.js

**Authentication lost**
- Delete profile and re-authenticate: `rm -rf ~/.pi/chrome-profile`
- Or restore from backup: `./set-cookies.js ~/.pi/jellyfish-auth.json`

## Architecture

These tools use Puppeteer Core to communicate with Chromium via the Chrome DevTools Protocol (CDP):

1. `start-browser.js` launches Chromium and saves the WebSocket endpoint
2. Other tools connect to the running browser via that endpoint
3. Browser stays open until you press Ctrl+C
4. All tools are independent - agent can call them in any order

This is simpler and more efficient than MCP servers (2-6 tools vs 26 tools, no context overhead).
