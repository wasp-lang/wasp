import { exit } from 'process';
import { $, question } from 'zx';
import { isYes, waspSays } from '../../../helpers.js';
import { Command } from 'commander';

export async function railwayExists(railwayExe: string): Promise<boolean> {
  try {
    await $`${railwayExe} -V`;
    return true;
  } catch {
    return false;
  }
}

export async function isUserLoggedIn(railwayExe: string): Promise<boolean> {
  try {
    await $`${railwayExe} whoami`;
    return true;
  } catch {
    return false;
  }
}

async function ensureUserLoggedIn(railwayExe: string): Promise<void> {
  const userLoggedIn = await isUserLoggedIn(railwayExe);
  if (userLoggedIn) {
    return;
  }

  const answer = await question(
    'railway is not logged into Railway. Would you like to log in now? ',
  );
  if (!isYes(answer)) {
    waspSays('Ok, exiting.');
    exit(1);
  }

  try {
    await $`${railwayExe} login`;
  } catch {
    waspSays('It seems there was a problem logging in. Please run "railway login" and try again.');
    exit(1);
  }
}

export async function ensureRailwayReady(thisCommand: Command): Promise<void> {
  const railwayExe = thisCommand.opts().railwayExe;
  const doesRailwayExist = await railwayExists(railwayExe);
  if (!doesRailwayExist) {
    waspSays('The Railway CLI is not available on this system.');
    waspSays('Please install the railway CLI here: https://railway.app/docs/cli');
    exit(1);
  }
  await ensureUserLoggedIn(railwayExe);
}

export async function getServiceUrl(
  railwayExe: string,
  serviceName: string,
  port: number,
): Promise<string> {
  const result = await $`${railwayExe} domain --service "${serviceName}" --port ${port}`;
  const match = result.stdout.match(/https:\/\/[^\s]*/);
  if (!match) throw new Error('Failed to get service domain');
  return match[0];
}

export async function getExistingProject(railwayExe: string): Promise<{
  projectName: string;
  serviceNames: string[];
} | null> {
  try {
    const result = await $`${railwayExe} status --json`;
    const json = JSON.parse(result.stdout);

    return {
      projectName: json.name,
      serviceNames: json.services.edges.map((edge: { node: { name: string } }) => edge.node.name),
    };
  } catch {
    return null;
  }
}
