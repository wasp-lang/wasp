import { Command } from 'commander';
import { exit } from 'process';
import { $, fs, question } from 'zx';
import {
    ensureDirAbsoluteAndExists,
    isYes,
    waspSays,
} from '../../shared/helpers.js';
import { RailwayDeploymentConfig } from '../types.js';
import { RAILWAY_CONFIG_FILE_NAME, RAILWAY_INSTALL_CLI_URL } from './consts.js';

export async function isUserLoggedIn(): Promise<boolean> {
    try {
        await $`railway whoami`;
        return true;
    } catch {
        return false;
    }
}

async function ensureUserLoggedIn(): Promise<void> {
    const userLoggedIn = await isUserLoggedIn();
    if (userLoggedIn) {
        return;
    }

    const answer = await question(
        'You are not logged in to Railway CLI. Would you like to log in now? ',
    );
    if (!isYes(answer)) {
        waspSays('Ok, exiting.');
        exit(1);
    }

    try {
        await $`railway login`;
    } catch {
        waspSays(
            'It seems there was a problem logging in. Please run "railway login" and try again.',
        );
        exit(1);
    }
}

export async function railwayCliExists(): Promise<boolean> {
    try {
        await $`railway -V`;
        return true;
    } catch {
        return false;
    }
}

export async function ensureRailwayReady(): Promise<void> {
    const doesRailwayCliExist = await railwayCliExists();
    if (!doesRailwayCliExist) {
        waspSays('The Railway CLI is not available on this system.');
        waspSays(
            'Please install the Railway CLI here: ' + RAILWAY_INSTALL_CLI_URL,
        );
        exit(1);
    }
    await ensureUserLoggedIn();
}

export function ensureDirsInCmdAreAbsoluteAndPresent(
    thisCommand: Command,
): void {
    const waspProjectDirPath: string | undefined = thisCommand.opts()
        .waspProjectDir;
    ensureDirAbsoluteAndExists({ label: 'Wasp dir', dir: waspProjectDirPath });
}

// TODO: add support for multiple environmental files (prod, dev)
// with flag for user to pass which config file to use
export function getRailwayConfig(): RailwayDeploymentConfig {
    return fs.readJsonSync(RAILWAY_CONFIG_FILE_NAME, 'utf8');
}

async function hasLinkedProject() {
    try {
        await $`railway status`;
        return true;
    } catch {
        return false;
    }
}

export async function ensureProjectLinked({
    environment,
    projectId,
}: Pick<RailwayDeploymentConfig, 'environment' | 'projectId'>) {
    const isLinked = await hasLinkedProject();
    if (isLinked) {
        return;
    }
    await $`railway link ${projectId} --environment ${environment} `;
    waspSays(`Project ${projectId} linked!`);
}
