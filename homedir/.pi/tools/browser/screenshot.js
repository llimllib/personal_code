#!/usr/bin/env node
/**
 * Take a screenshot of the current browser page.
 * 
 * Usage:
 *   ./screenshot.js [output-path]
 *   ./screenshot.js                      # Saves to /tmp/screenshot-<timestamp>.png
 *   ./screenshot.js ~/Desktop/page.png   # Saves to specific path
 *   ./screenshot.js --fullpage           # Full page screenshot (not just viewport)
 * 
 * Examples:
 *   ./screenshot.js
 *   ./screenshot.js /tmp/issues.png
 *   ./screenshot.js --fullpage
 */

import puppeteer from 'puppeteer-core';
import { readFileSync } from 'fs';

const ENDPOINT_FILE = '/tmp/browser-endpoint.txt';

const args = process.argv.slice(2);
const fullPage = args.includes('--fullpage');
const outputPath = args.find(arg => !arg.startsWith('--')) || 
                  `/tmp/screenshot-${Date.now()}.png`;

try {
  const wsEndpoint = readFileSync(ENDPOINT_FILE, 'utf-8').trim();
  const browser = await puppeteer.connect({ browserWSEndpoint: wsEndpoint, defaultViewport: null });
  const pages = await browser.pages();
  const page = pages[pages.length - 1]; // Use most recent tab
  
  console.log(`Taking screenshot...`);
  console.log(`  URL: ${page.url()}`);
  console.log(`  Full page: ${fullPage}`);
  
  await page.screenshot({ 
    path: outputPath,
    fullPage: fullPage,
  });
  
  console.log(`✓ Screenshot saved: ${outputPath}`);
  
  await browser.disconnect();
} catch (error) {
  console.error('Error:', error.message);
  if (error.message.includes('ENOENT')) {
    console.error('\nBrowser not running. Start it with: ./start-browser.js');
  }
  process.exit(1);
}
