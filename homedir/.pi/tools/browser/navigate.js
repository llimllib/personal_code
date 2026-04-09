#!/usr/bin/env node
/**
 * Navigate browser to a URL.
 * 
 * Usage:
 *   ./navigate.js <url>              # Navigate in current tab
 *   ./navigate.js <url> --new-tab    # Navigate in new tab
 * 
 * Examples:
 *   ./navigate.js http://localhost:3000
 *   ./navigate.js http://localhost:3000/issues --new-tab
 */

import puppeteer from 'puppeteer-core';
import { readFileSync } from 'fs';

const ENDPOINT_FILE = '/tmp/browser-endpoint.txt';

const args = process.argv.slice(2);
const url = args[0];
const newTab = args.includes('--new-tab');

if (!url) {
  console.error('Usage: ./navigate.js <url> [--new-tab]');
  process.exit(1);
}

try {
  const wsEndpoint = readFileSync(ENDPOINT_FILE, 'utf-8').trim();
  const browser = await puppeteer.connect({ browserWSEndpoint: wsEndpoint, defaultViewport: null });
  
  let page;
  if (newTab) {
    page = await browser.newPage();
  } else {
    const pages = await browser.pages();
    page = pages[pages.length - 1]; // Use most recent tab
  }
  
  console.log(`Navigating to: ${url}`);
  await page.goto(url, { waitUntil: 'networkidle2' });
  console.log(`✓ Navigation complete`);
  console.log(`  Title: ${await page.title()}`);
  console.log(`  URL: ${page.url()}`);
  
  await browser.disconnect();
} catch (error) {
  console.error('Error:', error.message);
  if (error.message.includes('ENOENT')) {
    console.error('\nBrowser not running. Start it with: ./start-browser.js');
  }
  process.exit(1);
}
