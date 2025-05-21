import fs from 'fs';
import path from 'node:path';

import { Command } from 'commander';
import { exit } from 'process';
import { cd, chalk } from 'zx';

export function isYes(str: string): boolean {
  return str.trim().toLowerCase().startsWith('y');
}

export function ensureWaspDirLooksRight(thisCommand: Command): void {
  const dirContainsWasproot = fs.existsSync(
    path.join(thisCommand.opts().waspProjectDir, '.wasproot'),
  );
  if (dirContainsWasproot) {
    return;
  }

  waspSays('The supplied Wasp directory does not appear to be a valid Wasp project.');
  waspSays('Please double check your path.');
  exit(1);
}

export function buildDirExists(waspProjectDir: string): boolean {
  const waspBuildDir = getWaspBuildDir(waspProjectDir);
  return fs.existsSync(waspBuildDir);
}

export function cdToServerBuildDir(waspProjectDir: string): void {
  const serverBuildDir = getServerBuildDir(waspProjectDir);
  cd(serverBuildDir);
}

export function cdToClientBuildDir(waspProjectDir: string): void {
  const webAppBuildDir = getWebAppBuildDir(waspProjectDir);
  cd(webAppBuildDir);
}

export function getServerArtefactsDir(waspProjectDir: string): string {
  return getServerBuildDir(waspProjectDir);
}

export function getWebAppArtefactsDir(waspProjectDir: string): string {
  const webAppBuildDir = getWebAppBuildDir(waspProjectDir);
  return path.join(webAppBuildDir, 'build');
}

function getWaspBuildDir(waspProjectDir: string): string {
  return path.join(waspProjectDir, '.wasp', 'build');
}

function getServerBuildDir(waspProjectDir: string): string {
  return getWaspBuildDir(waspProjectDir);
}

function getWebAppBuildDir(waspProjectDir: string): string {
  return path.join(getWaspBuildDir(waspProjectDir), 'web-app');
}

export function ensureWaspProjectDirInCmdIsAbsoluteAndPresent(thisCommand: Command): void {
  const waspProjectDirPath: string | undefined = thisCommand.opts().waspProjectDir;
  if (waspProjectDirPath) {
    if (!path.isAbsolute(waspProjectDirPath)) {
      waspSays('The Wasp dir path must be absolute.');
      exit(1);
    }

    const waspProjectDirExists = fs.existsSync(waspProjectDirPath);
    if (!waspProjectDirExists) {
      waspSays('The Wasp dir path does not exist.');
      exit(1);
    }
  }
}

// eslint-disable-next-line
export function makeIdempotent<F extends () => any>(fn: F): () => ReturnType<F> {
  let result: { value: ReturnType<F> } | null = null;

  return function idempotentFn() {
    if (!result) {
      result = { value: fn() };
    }
    return result.value;
  };
}

export function waspSays(str: string): void {
  console.log(chalk.yellow(`🚀 ${str}`));
}

export function boldText(str: string): string {
  return chalk.bold(str);
}

export function displayWaspRocketImage(): void {
  // Escaping backslashes makes it look weird here, but it works in console.
  const asciiArt = `

                    __
                   // \\
                   \\\\_/ //
             _    -(||)(')
            \\ \\____///_____
   #########[==__DEPLOYED__}
            /_/

  `;
  console.log(asciiArt);
}

export function getCommandHelp(command: Command): string {
  return trimUsage(command.helpInformation());
}

function trimUsage(usage: string): string {
  return usage
    .split(/[\r\n]+/)[0]
    .replace('Usage: ', '')
    .replace(' [options]', '');
}
