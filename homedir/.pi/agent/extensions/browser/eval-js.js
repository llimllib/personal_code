#!/usr/bin/env node
/**
 * Execute JavaScript in the current browser page.
 * 
 * Usage:
 *   ./eval-js.js "<javascript>"
 *   ./eval-js.js --file <path>
 * 
 * Examples:
 *   ./eval-js.js "document.title"
 *   ./eval-js.js "document.querySelectorAll('.issue').length"
 *   ./eval-js.js "window.location.href"
 *   ./eval-js.js --file test-script.js
 * 
 * The result is printed as JSON.
 */

import puppeteer from 'puppeteer-core';
import { readFileSync } from 'fs';

const ENDPOINT_FILE = '/tmp/browser-endpoint.txt';

const args = process.argv.slice(2);
let script;

if (args[0] === '--file') {
  if (!args[1]) {
    console.error('Usage: ./eval-js.js --file <path>');
    process.exit(1);
  }
  script = readFileSync(args[1], 'utf-8');
} else {
  script = args[0];
}

if (!script) {
  console.error('Usage: ./eval-js.js "<javascript>" or ./eval-js.js --file <path>');
  process.exit(1);
}

try {
  const wsEndpoint = readFileSync(ENDPOINT_FILE, 'utf-8').trim();
  const browser = await puppeteer.connect({ 
    browserWSEndpoint: wsEndpoint,
    defaultViewport: null
  });
  const pages = await browser.pages();
  const page = pages[pages.length - 1]; // Use most recent tab
  
  const result = await page.evaluate((script) => {
    // eslint-disable-next-line no-eval
    return eval(script);
  }, script);
  
  console.log(JSON.stringify(result, null, 2));
  
  await browser.disconnect();
} catch (error) {
  console.error('Error:', error.message);
  if (error.message.includes('ENOENT')) {
    console.error('\nBrowser not running. Start it with: ./start-browser.js');
  }
  process.exit(1);
}
