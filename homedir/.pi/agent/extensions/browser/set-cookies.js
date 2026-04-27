#!/usr/bin/env node
/**
 * Set cookies in the current browser page.
 * Useful for restoring authentication state.
 * 
 * Usage:
 *   ./set-cookies.js <cookies-file>
 * 
 * Examples:
 *   ./set-cookies.js ~/.pi/jellyfish-auth.json
 *   ./set-cookies.js /tmp/cookies.json
 */

import puppeteer from 'puppeteer-core';
import { readFileSync } from 'fs';

const ENDPOINT_FILE = '/tmp/browser-endpoint.txt';

const args = process.argv.slice(2);
const cookiesFile = args[0];

if (!cookiesFile) {
  console.error('Usage: ./set-cookies.js <cookies-file>');
  process.exit(1);
}

try {
  const wsEndpoint = readFileSync(ENDPOINT_FILE, 'utf-8').trim();
  const cookies = JSON.parse(readFileSync(cookiesFile, 'utf-8'));
  
  const browser = await puppeteer.connect({ browserWSEndpoint: wsEndpoint, defaultViewport: null });
  const pages = await browser.pages();
  const page = pages[pages.length - 1];
  
  await page.setCookie(...cookies);
  
  console.log(`✓ Set ${cookies.length} cookies`);
  
  await browser.disconnect();
} catch (error) {
  console.error('Error:', error.message);
  if (error.message.includes('ENOENT')) {
    if (error.message.includes(ENDPOINT_FILE)) {
      console.error('\nBrowser not running. Start it with: ./start-browser.js');
    } else {
      console.error(`\nCookies file not found: ${cookiesFile}`);
    }
  }
  process.exit(1);
}
