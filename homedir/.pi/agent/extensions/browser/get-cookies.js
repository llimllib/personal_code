#!/usr/bin/env node
/**
 * Get cookies from the current browser page.
 * Useful for saving authentication state.
 * 
 * Usage:
 *   ./get-cookies.js [output-file]
 *   ./get-cookies.js                           # Print to stdout
 *   ./get-cookies.js ~/.pi/jellyfish-auth.json # Save to file
 * 
 * Examples:
 *   ./get-cookies.js > /tmp/cookies.json
 *   ./get-cookies.js ~/.pi/jellyfish-auth.json
 */

import puppeteer from 'puppeteer-core';
import { readFileSync, writeFileSync } from 'fs';

const ENDPOINT_FILE = '/tmp/browser-endpoint.txt';

const args = process.argv.slice(2);
const outputFile = args[0];

try {
  const wsEndpoint = readFileSync(ENDPOINT_FILE, 'utf-8').trim();
  const browser = await puppeteer.connect({ browserWSEndpoint: wsEndpoint, defaultViewport: null });
  const pages = await browser.pages();
  const page = pages[pages.length - 1];
  
  const cookies = await page.cookies();
  const json = JSON.stringify(cookies, null, 2);
  
  if (outputFile) {
    writeFileSync(outputFile, json);
    console.error(`✓ Saved ${cookies.length} cookies to: ${outputFile}`);
  } else {
    console.log(json);
  }
  
  await browser.disconnect();
} catch (error) {
  console.error('Error:', error.message);
  if (error.message.includes('ENOENT')) {
    console.error('\nBrowser not running. Start it with: ./start-browser.js');
  }
  process.exit(1);
}
