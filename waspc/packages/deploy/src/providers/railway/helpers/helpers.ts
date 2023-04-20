import { exit } from 'process';
import { $, question } from 'zx';
import { isYes, waspSays } from '../../shared/helpers.js';

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

const RAILWAY_INSTALL_CLI_URL = 'https://docs.railway.app/develop/cli';
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
