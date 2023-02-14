import { exit } from 'process';
import { $, question } from 'zx';
import { executeFlyCommand } from '../index.js';
import { silence, isYes, waspSays, getCommandHelp } from './helpers.js';

export async function flyctlExists(): Promise<boolean> {
	try {
		await $`flyctl version`;
		return true;
	} catch {
		return false;
	}
}

export async function isUserLoggedIn(): Promise<boolean> {
	try {
		await $`flyctl auth whoami`;
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

	const answer = await question('flyctl is not logged into Fly.io. Would you like to log in now? ');
	if (!isYes(answer)) {
		waspSays('Ok, exiting.');
		exit(1);
	}

	try {
		await $`flyctl auth login`;
	} catch {
		waspSays('It seems there was a problem logging in. Please run "flyctl auth login" and try again.');
		exit(1);
	}

}

export async function ensureFlyReady(): Promise<void> {
	const doesFlyctlExist = await flyctlExists();
	if (!doesFlyctlExist) {
		waspSays('The Fly.io CLI is not available on this system.');
		waspSays('Please install the flyctl here: https://fly.io/docs/hands-on/install-flyctl');
		exit(1);
	}
	await ensureUserLoggedIn();
}

export async function ensureRegionIsValid(region: string): Promise<void> {
	try {
		const validRegion = await regionExists(region);
		if (!validRegion) {
			waspSays(`Invalid region code ${region}. Please specify a valid 3 character region id: https://fly.io/docs/reference/regions`);
			waspSays(`You can also run "${getCommandHelp(executeFlyCommand).replace('<cmd...>', 'platform regions --context server')}".`);
			exit(1);
		}
	} catch (e) {
		// Ignore any errors while checking. Commands requiring a valid region will still fail if invalid, just not as nicely.
		console.error(e);
		waspSays('Unable to validate region before calling flyctl.');
	}
}

async function regionExists(regionCode: string): Promise<boolean> {
	const proc = await silence(($hh) => $hh`flyctl platform regions -j`);
	const regions: { Code: string; Name: string }[] = JSON.parse(proc.stdout);
	return regions.some(r => r.Code === regionCode);
}

export async function secretExists(secretName: string): Promise<boolean> {
	const proc = await $`flyctl secrets list -j`;
	const secrets: { Name: string }[] = JSON.parse(proc.stdout);
	return secrets.some(s => s.Name === secretName);
}
