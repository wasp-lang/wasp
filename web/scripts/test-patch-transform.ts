#!/usr/bin/env tsx

import { readFileSync } from 'fs';
import { join } from 'path';
import { transformPatch, transformPatchFile, findPatchFile } from '../src/utils/patch-transformer';

function main() {
  const args = process.argv.slice(2);
  
  if (args.length < 1) {
    console.error('Usage: tsx test-patch-transform.ts <patch-id> [lang]');
    console.error('       tsx test-patch-transform.ts <patch-file-path> [lang]');
    console.error('');
    console.error('Examples:');
    console.error('  tsx test-patch-transform.ts prepare-project tsx');
    console.error('  tsx test-patch-transform.ts query-get-tasks wasp');
    console.error('  tsx test-patch-transform.ts ./patches/05-queries__query-get-tasks.patch wasp');
    process.exit(1);
  }

  const inputArg = args[0];
  const lang = args[1] || 'ts';
  
  let patchPath: string;
  
  if (inputArg.includes('/') || inputArg.endsWith('.patch')) {
    // Direct file path
    patchPath = inputArg;
  } else {
    // Patch ID - find the file
    const patchesDir = join(__dirname, '../docs/tutorial/patches');
    const foundPath = findPatchFile(patchesDir, inputArg);
    
    if (!foundPath) {
      console.error(`Patch file not found for id: ${inputArg}`);
      process.exit(1);
    }
    
    patchPath = foundPath;
  }

  try {
    console.log(`\n=== Transforming patch: ${patchPath} ===`);
    console.log(`Language: ${lang}`);
    console.log(`\n--- Original patch content ---`);
    
    const patchContent = readFileSync(patchPath, 'utf-8');
    console.log(patchContent);
    
    console.log(`\n--- Transformed output ---`);
    const transformed = transformPatchFile(patchPath, lang);
    console.log(transformed);
    
    console.log(`\n=== Done ===`);
  } catch (error) {
    console.error('Error:', error);
    process.exit(1);
  }
}

if (require.main === module) {
  main();
}