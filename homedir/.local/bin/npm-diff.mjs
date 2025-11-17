#!/usr/bin/env node

import { exec } from 'child_process';
import { promisify } from 'util';
import path from 'node:path';

const execAsync = promisify(exec);

const [dirA = '.', dirB] = process.argv.slice(2);

if (!dirB) {
  console.error('Usage: npm-diff <dir-a> <dir-b>');
  console.error('Example: npm-diff . ../next');
  process.exit(1);
}

// ANSI color codes
const colors = {
  red: '\x1b[31m',
  green: '\x1b[32m',
  reset: '\x1b[0m',
};

async function getInstalledPackages(dir) {
  try {
    const resolvedPath = path.resolve(dir);
    // npm list --json --all shows ALL installed packages (including nested)
    const { stdout } = await execAsync('npm list --json --all', {
      cwd: resolvedPath,
      maxBuffer: 10 * 1024 * 1024, // 10MB buffer
    });
    const data = JSON.parse(stdout);
    return data;
  } catch (error) {
    // npm list exits with code 1 if there are issues, but still outputs JSON
    if (error.stdout) {
      try {
        const data = JSON.parse(error.stdout);
        return data;
      } catch {
        // fall through to error below
      }
    }
    throw new Error(`Failed to get installed packages in ${dir}: ${error.message}`);
  }
}

function extractAllVersions(data) {
  const versions = {};

  function traverse(deps) {
    if (!deps) return;

    for (const [pkg, info] of Object.entries(deps)) {
      if (info.version) {
        // Keep track of all versions we see for a package
        if (!versions[pkg]) {
          versions[pkg] = new Set();
        }
        versions[pkg].add(info.version);
      }
      // Recursively traverse nested dependencies
      if (info.dependencies) {
        traverse(info.dependencies);
      }
    }
  }

  traverse(data.dependencies);

  return versions;
}

function compareDependencies(versionsA, versionsB) {
  const allPackages = new Set([...Object.keys(versionsA), ...Object.keys(versionsB)]);
  const differences = [];

  for (const pkg of allPackages) {
    const setA = versionsA[pkg] || new Set();
    const setB = versionsB[pkg] || new Set();

    // Check if there are any differences
    const allVersions = new Set([...setA, ...setB]);
    let hasDifference = false;

    for (const version of allVersions) {
      if (!setA.has(version) || !setB.has(version)) {
        hasDifference = true;
        break;
      }
    }

    if (hasDifference) {
      differences.push({
        package: pkg,
        versionsA: setA,
        versionsB: setB,
      });
    }
  }

  return differences.sort((a, b) => a.package.localeCompare(b.package));
}

function colorizeVersions(versionsA, versionsB) {
  const arrayA = Array.from(versionsA).sort();
  const arrayB = Array.from(versionsB).sort();

  // Color versions in A (red if not in B)
  const coloredA = arrayA.map(v => (versionsB.has(v) ? v : `${colors.red}${v}${colors.reset}`)).join(', ');

  // Color versions in B (green if not in A)
  const coloredB = arrayB.map(v => (versionsA.has(v) ? v : `${colors.green}${v}${colors.reset}`)).join(', ');

  return {
    coloredA: coloredA || '-',
    coloredB: coloredB || '-',
    plainA: arrayA.join(', ') || '-',
    plainB: arrayB.join(', ') || '-',
  };
}

function printTable(differences, dirA, dirB) {
  if (differences.length === 0) {
    console.log('No differences found.');
    return;
  }

  // Calculate column widths based on plain text
  const maxPackageLength = Math.max('Package'.length, ...differences.map(d => d.package.length));

  const plainVersions = differences.map(d => colorizeVersions(d.versionsA, d.versionsB));
  const maxVersionALength = Math.max(dirA.length, ...plainVersions.map(v => v.plainA.length));
  const maxVersionBLength = Math.max(dirB.length, ...plainVersions.map(v => v.plainB.length));

  const pad = (str, length, plainLength) => {
    const diff = str.length - plainLength;
    return str + ' '.repeat(Math.max(0, length - plainLength));
  };

  // Print header
  console.log(
    `${pad('Package', maxPackageLength, 'Package'.length)}  ${pad(dirA, maxVersionALength, dirA.length)}  ${pad(dirB, maxVersionBLength, dirB.length)}`,
  );

  // Print separator
  console.log(`${'-'.repeat(maxPackageLength)}  ${'-'.repeat(maxVersionALength)}  ${'-'.repeat(maxVersionBLength)}`);

  // Print rows
  for (const { package: pkg, versionsA, versionsB } of differences) {
    const { coloredA, coloredB, plainA, plainB } = colorizeVersions(versionsA, versionsB);

    console.log(
      `${pad(pkg, maxPackageLength, pkg.length)}  ${pad(coloredA, maxVersionALength, plainA.length)}  ${pad(coloredB, maxVersionBLength, plainB.length)}`,
    );
  }
}

async function main() {
  try {
    const [installedA, installedB] = await Promise.all([getInstalledPackages(dirA), getInstalledPackages(dirB)]);

    const versionsA = extractAllVersions(installedA);
    const versionsB = extractAllVersions(installedB);

    const differences = compareDependencies(versionsA, versionsB);

    printTable(differences, dirA, dirB);
  } catch (error) {
    console.error(`Error: ${error.message}`);
    process.exit(1);
  }
}

main();
