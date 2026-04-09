# Browser Tools Quick Reference

## Setup (One-time)
```bash
cd .pi/tools/browser
npm install
chmod +x *.js
```

## Start Browser
```bash
./start-browser.js                    # With persistent profile (auth preserved)
./start-browser.js --fresh            # Fresh profile (no auth)
./start-browser.js --url <url>        # Start and navigate
```

## Navigate
```bash
./navigate.js http://localhost:3000
./navigate.js http://localhost:3000/issues --new-tab
```

## Execute JavaScript
```bash
./eval-js.js "document.title"
./eval-js.js "document.querySelectorAll('.issue').length"
./eval-js.js "document.querySelector('.error')?.textContent"
./eval-js.js --file script.js
```

## Screenshot
```bash
./screenshot.js                       # Auto-named in /tmp
./screenshot.js /tmp/page.png
./screenshot.js --fullpage
```

## Console Messages
```bash
./get-console.js                      # Show console access info
./get-console.js --follow             # Follow console output (Ctrl+C to stop)
```

## Cookies (Authentication)
```bash
./get-cookies.js ~/.pi/jellyfish-auth.json
./set-cookies.js ~/.pi/jellyfish-auth.json
```

## Common Tasks

**Check page loaded:**
```bash
./eval-js.js "document.readyState"
```

**Count elements:**
```bash
./eval-js.js "document.querySelectorAll('.item').length"
```

**Get element text:**
```bash
./eval-js.js "document.querySelector('.status')?.textContent"
```

**Check for errors:**
```bash
./eval-js.js "!!document.querySelector('.error-message')"
```

**Click button:**
```bash
./eval-js.js "document.querySelector('.button').click(); 'clicked'"
```

**Fill form:**
```bash
./eval-js.js "document.querySelector('#input').value = 'test'; 'filled'"
```

**Get current URL:**
```bash
./eval-js.js "window.location.href"
```

## Debugging Workflow

1. Start browser: `./start-browser.js`
2. Navigate: `./navigate.js http://localhost:3000`
3. Log in manually (if needed)
4. Test: `./eval-js.js "..."`
5. Screenshot: `./screenshot.js /tmp/state.png`

## Files

- **Profile:** `~/.pi/chrome-profile`
- **WebSocket:** `/tmp/browser-endpoint.txt`
- **Skill:** `~/.pi/agent/skills/web-debug/SKILL.md`

## Troubleshooting

| Issue | Solution |
|-------|----------|
| "Browser not running" | `./start-browser.js` |
| "Cannot find Chromium" | Check `/Applications/Chromium.app` |
| Auth expired | Delete `~/.pi/chrome-profile`, re-login |
| Tools not working | `chmod +x *.js` |
