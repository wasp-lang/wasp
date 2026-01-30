// @ts-check

// Parses an env file and outputs Wasp deploy server-secret arguments to stdout.
// Requires Node.js 20.12.0+ for util.parseEnv.
// Called from: .github/workflows/ci-deploy-test.yaml

import { readFileSync } from "node:fs";
import { parseEnv } from "node:util";

const envFilePath = parseArgs();
const envVars = readAndParseEnvFile(envFilePath);
outputSecrets(envVars);

/**
 * @returns {string} Path to the env file
 */
function parseArgs() {
  const args = process.argv.slice(2);

  if (args.length !== 1) {
    console.error("Usage: node prepare-deploy-secrets.mjs <env-file-path>");
    throw new Error("Invalid number of arguments");
  }

  return args[0];
}

/**
 * @param {string} filePath - Path to the env file
 * @returns {NodeJS.Dict<string>} Parsed environment variables
 */
function readAndParseEnvFile(filePath) {
  const content = readFileSync(filePath, "utf-8");
  return parseEnv(content);
}

/**
 * Outputs secrets in the format expected by Wasp deploy commands.
 * Uses null-byte delimiters to properly handle multiline values and special characters.
 * @param {NodeJS.Dict<string>} envVars - Environment variables to output
 */
function outputSecrets(envVars) {
  for (const [key, value] of Object.entries(envVars)) {
    // Skip empty env var values
    if (!value) {
      continue;
    }
    process.stdout.write("--server-secret\0");
    process.stdout.write(`${key}=${value}\0`);
  }
}
