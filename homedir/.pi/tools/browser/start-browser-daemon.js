#!/usr/bin/env node
/**
 * Browser daemon - runs browser and keeps it alive.
 * This is meant to be called by start-browser-bg.sh via nohup.
 * Use start-browser.js directly for foreground/interactive use.
 */

import puppeteer from 'puppeteer-core';
import { writeFileSync } from 'fs';
import { homedir } from 'os';
import { join } from 'path';

const CHROMIUM_PATH = '/Applications/Chromium.app/Contents/MacOS/Chromium';
const PROFILE_DIR = join(homedir(), '.pi', 'chrome-profile');
const ENDPOINT_FILE = '/tmp/browser-endpoint.txt';

const args = process.argv.slice(2);
const useFreshProfile = args.includes('--fresh');
const urlIndex = args.indexOf('--url');
const startUrl = urlIndex !== -1 ? args[urlIndex + 1] : 'about:blank';

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

console.log(`Browser started: ${wsEndpoint}`);

// Navigate to URL if provided
if (startUrl !== 'about:blank') {
  const pages = await browser.pages();
  const page = pages[0];
  console.log(`Navigating to: ${startUrl}`);
  await page.goto(startUrl);
}

// Keep process alive
process.stdin.resume();

// Graceful shutdown on SIGTERM
process.on('SIGTERM', async () => {
  console.log('Received SIGTERM, closing browser...');
  await browser.close();
  process.exit(0);
});
