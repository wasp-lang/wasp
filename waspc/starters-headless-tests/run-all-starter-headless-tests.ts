#!/usr/bin/env -S node --no-warnings --loader ts-node/esm

import { spawn } from 'child_process';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

type StarterHeadlessTest = 
  | LocalStarterHeadlessTest 
  | RemoteStarterHeadlessTest;

type LocalStarterHeadlessTest = {
  templateName: string;
  appRelativePath: string;
};

type RemoteStarterHeadlessTest = LocalStarterHeadlessTest & {
  headlessTestsRelativePath: string;
}

const minimalStarterHeadlessTest: LocalStarterHeadlessTest = {
  templateName: 'minimal',
  appRelativePath: '/',
};

const basicStarterHeadlessTest: LocalStarterHeadlessTest = {
  templateName: 'basic',
  appRelativePath: '/',
};

const openSaasStarterHeadlessTest: RemoteStarterHeadlessTest = {
  templateName: 'saas',
  appRelativePath: '/app',
  headlessTestsRelativePath: '/e2e-tests',
};

const headlessTests: StarterHeadlessTest[] = [
  // minimalStarterHeadlessTest,
  // basicStarterHeadlessTest,
  openSaasStarterHeadlessTest,
];

async function runAllHeadlessTests(): Promise<void> {
  const { waspCliCommand } = parseArgs();
  
  for (const test of headlessTests) {
    try {
      await runStarterHeadlessTest(waspCliCommand, test);
    } catch (error) {
      console.error(`Failed to run headless test for ${test.templateName}:`, error);
      process.exit(1);
    }
  }
}

type Args = {
  waspCliCommand: "wasp" | "wasp-cli";
}

function parseArgs(): Args {
  const [_node, _script, ...userArgs] = process.argv;
  
  if (userArgs.length === 0) {
    console.error('Error: Please provide Wasp CLI command ("wasp" or "wasp-cli")');
    process.exit(1);
  }
  
  const waspCliCommand = userArgs[0];
  
  if (waspCliCommand !== 'wasp' && waspCliCommand !== 'wasp-cli') {
    console.error('Error: Wasp CLI command must be either "wasp" or "wasp-cli"');
    process.exit(1);
  }

  return { waspCliCommand };
}

async function runStarterHeadlessTest(
  waspCliCommand: string,
  test: StarterHeadlessTest
): Promise<void> {
  const args = [
    waspCliCommand,
    test.templateName,
    test.appRelativePath,
  ];
  
  if ('headlessTestsRelativePath' in test) {
    args.push(test.headlessTestsRelativePath);
  }
  
  console.log(`Running headless test for ${test.templateName}...`);
  
  const scriptDirectory = dirname(fileURLToPath(import.meta.url));
  const testRunnerScriptPath = join(scriptDirectory, 'run-starter-headless-tests.sh');
  
  const childProcess = spawn(testRunnerScriptPath, args, {
    stdio: 'inherit',
    shell: true,
  });
  
  return new Promise((resolve, reject) => {
    childProcess.on('close', (code) => {
      if (code === 0) {
        console.log(`✅ Headless test for ${test.templateName} completed successfully`);
        resolve();
      } else {
        console.error(`❌ Headless test for ${test.templateName} failed with exit code ${code}`);
        reject(new Error(`Test failed with exit code ${code}`));
      }
    });
    
    childProcess.on('error', (error) => {
      console.error(`❌ Failed to start headless test for ${test.templateName}:`, error);
      reject(error);
    });
  });
}

runAllHeadlessTests().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});