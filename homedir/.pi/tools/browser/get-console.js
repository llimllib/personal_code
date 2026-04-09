#!/usr/bin/env node
/**
 * Get console messages from the current browser page.
 * 
 * Usage:
 *   ./get-console.js              # Get recent console messages
 *   ./get-console.js --follow     # Follow console output (Ctrl+C to stop)
 * 
 * Examples:
 *   ./get-console.js
 *   ./get-console.js --follow
 */

import puppeteer from 'puppeteer-core';
import { readFileSync } from 'fs';

const ENDPOINT_FILE = '/tmp/browser-endpoint.txt';

const args = process.argv.slice(2);
const follow = args.includes('--follow');

try {
  const wsEndpoint = readFileSync(ENDPOINT_FILE, 'utf-8').trim();
  const browser = await puppeteer.connect({ browserWSEndpoint: wsEndpoint, defaultViewport: null });
  const pages = await browser.pages();
  const page = pages[pages.length - 1]; // Use most recent tab
  
  if (follow) {
    console.log('Following console output (Ctrl+C to stop)...\n');
    
    page.on('console', msg => {
      const type = msg.type();
      const text = msg.text();
      const location = msg.location();
      console.log(`[${type.toUpperCase()}] ${text}`);
      if (location.url) {
        console.log(`  at ${location.url}:${location.lineNumber}`);
      }
    });
    
    // Keep process alive
    process.stdin.resume();
    
    process.on('SIGINT', async () => {
      console.log('\nStopping...');
      await browser.disconnect();
      process.exit(0);
    });
  } else {
    // For non-follow mode, we need to evaluate to get existing console state
    // Unfortunately, there's no way to get past console messages without
    // having set up listeners beforehand. We can only capture new ones.
    console.log('Note: This tool can only capture NEW console messages.');
    console.log('Past console messages are not accessible.');
    console.log('Use --follow to capture messages as they happen.\n');
    
    console.log('To see console errors/warnings in the browser DevTools:');
    console.log('1. Open DevTools in the browser window (Cmd+Option+I)');
    console.log('2. Go to Console tab');
    console.log('\nOr use --follow mode and trigger the action again.');
    
    await browser.disconnect();
  }
} catch (error) {
  console.error('Error:', error.message);
  if (error.message.includes('ENOENT')) {
    console.error('\nBrowser not running. Start it with: ./start-browser-bg.sh');
  }
  process.exit(1);
}
