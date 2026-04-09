#!/usr/bin/env node
/**
 * Start Chromium browser with persistent profile for debugging Jellyfish.
 * 
 * Usage:
 *   ./start-browser.js              # Start with persistent profile (foreground)
 *   ./start-browser.js --detach     # Start in background, exit immediately
 *   ./start-browser.js --fresh      # Start with temporary profile
 *   ./start-browser.js --url <url>  # Start and navigate to URL
 *   ./start-browser.js --force      # Kill existing browser and start fresh
 * 
 * The browser stays open and other tools can connect to it via WebSocket.
 */

import puppeteer from 'puppeteer-core';
import { writeFileSync, readFileSync, existsSync, unlinkSync } from 'fs';
import { homedir } from 'os';
import { join } from 'path';
import { execSync } from 'child_process';

const CHROMIUM_PATH = '/Applications/Chromium.app/Contents/MacOS/Chromium';
const PROFILE_DIR = join(homedir(), '.pi', 'chrome-profile');
const ENDPOINT_FILE = '/tmp/browser-endpoint.txt';

const args = process.argv.slice(2);
const detach = args.includes('--detach');
const useFreshProfile = args.includes('--fresh');
const force = args.includes('--force');
const urlIndex = args.indexOf('--url');
const startUrl = urlIndex !== -1 ? args[urlIndex + 1] : 'about:blank';

// Check if browser is already running
async function checkExistingBrowser() {
  if (!existsSync(ENDPOINT_FILE)) {
    return null;
  }
  
  try {
    const wsEndpoint = readFileSync(ENDPOINT_FILE, 'utf-8').trim();
    const browser = await puppeteer.connect({ browserWSEndpoint: wsEndpoint, defaultViewport: null });
    await browser.disconnect();
    return wsEndpoint;
  } catch (error) {
    // Endpoint file exists but connection failed - clean up stale file
    unlinkSync(ENDPOINT_FILE);
    return null;
  }
}

// Kill existing browser process
function killExistingBrowser() {
  try {
    // Find and kill Chromium processes using our profile
    execSync(`pkill -f "${PROFILE_DIR}"`, { stdio: 'ignore' });
    // Wait a bit for process to die
    execSync('sleep 1');
    // Clean up endpoint file
    if (existsSync(ENDPOINT_FILE)) {
      unlinkSync(ENDPOINT_FILE);
    }
    console.log('✓ Killed existing browser');
  } catch (error) {
    // pkill returns non-zero if no processes found, which is fine
  }
}

const existingEndpoint = await checkExistingBrowser();

if (existingEndpoint && !force) {
  console.log('✓ Browser already running');
  console.log(`  WebSocket: ${existingEndpoint}`);
  console.log(`  Endpoint file: ${ENDPOINT_FILE}`);
  console.log('\nUse --force to kill and restart, or use other tools to interact with it.');
  process.exit(0);
}

if (force && existingEndpoint) {
  console.log('Killing existing browser...');
  killExistingBrowser();
}

console.log('Starting Chromium...');

const browser = await puppeteer.launch({
  executablePath: CHROMIUM_PATH,
  headless: false,
  userDataDir: useFreshProfile ? undefined : PROFILE_DIR,
  defaultViewport: null,
  args: [
    '--no-first-run',
    '--no-default-browser-check',
    '--window-size=1920,1080',
  ],
});

const wsEndpoint = browser.wsEndpoint();
writeFileSync(ENDPOINT_FILE, wsEndpoint);

console.log(`✓ Browser started`);
console.log(`  WebSocket: ${wsEndpoint}`);
console.log(`  Profile: ${useFreshProfile ? 'temporary' : PROFILE_DIR}`);
console.log(`  Endpoint file: ${ENDPOINT_FILE}`);

// Navigate to URL if provided
if (startUrl !== 'about:blank') {
  const pages = await browser.pages();
  const page = pages[0];
  console.log(`  Navigating to: ${startUrl}`);
  await page.goto(startUrl);
}

if (detach) {
  console.log('\nBrowser running in background. Close browser window to exit.');
  // Disconnect from the browser so it keeps running when Node exits
  browser.disconnect();
  process.exit(0);
} else {
  console.log('\nPress Ctrl+C to close browser');
  
  // Graceful shutdown
  process.on('SIGINT', async () => {
    console.log('\nClosing browser...');
    await browser.close();
    process.exit(0);
  });

  // Keep process alive
  process.stdin.resume();
}
