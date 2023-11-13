import { Command } from 'commander';
import { exit } from 'process';
import { $, cd, ProcessOutput, Shell } from 'zx';
import fs from 'fs';
import path from 'node:path';

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

	waspSays(
		'The supplied Wasp directory does not appear to be a valid Wasp project.',
	);
	waspSays('Please double check your path.');
	exit(1);
}

export function buildDirExists(waspProjectDir: string): boolean {
	const waspBuildDir = getWaspBuildDir(waspProjectDir);
	return fs.existsSync(waspBuildDir);
}

export function cdToServerBuildDir(waspProjectDir: string): void {
	const waspBuildDir = getWaspBuildDir(waspProjectDir);
	cd(waspBuildDir);
}

export function cdToClientBuildDir(waspProjectDir: string): void {
	const waspBuildDir = getWaspBuildDir(waspProjectDir);
	cd(path.join(waspBuildDir, 'web-app'));
}

function getWaspBuildDir(waspProjectDir: string) {
	return path.join(waspProjectDir, '.wasp', 'build');
}

export function ensureDirsInCmdAreAbsoluteAndPresent(
	thisCommand: Command,
): void {
	const waspProjectDirPath: string | undefined =
		thisCommand.opts().waspProjectDir;
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

	const flyTomlDirPath: string | undefined = thisCommand.opts().flyTomlDir;
	if (flyTomlDirPath) {
		if (!path.isAbsolute(flyTomlDirPath)) {
			waspSays('The toml dir path must be absolute.');
			exit(1);
		}

		const flyTomlDirExists = fs.existsSync(flyTomlDirPath);
		if (!flyTomlDirExists) {
			waspSays('The toml dir path does not exist.');
			exit(1);
		}
	}
}

// eslint-disable-next-line
export function makeIdempotent<F extends () => any>(
	fn: F,
): () => ReturnType<F> {
	let result: { value: ReturnType<F> } | null = null;

	return function idempotentFn() {
		if (!result) {
			result = { value: fn() };
		}
		return result.value;
	};
}

// For some reason, the colors from the chalk package wouldn't
// show up when run as a subprocess by the Wasp CLI. This works.
export function waspSays(str: string): void {
	console.log('🚀 \x1b[33m ' + str + ' \x1b[0m');
}

export function boldText(str: string): string {
	return '\x1b[1m' + str + '\x1b[0m';
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

// There is a theoretical race condition here since we are modifying a global `$`
// property, that when we yield to the `await cmd($)` call that some other calls to
// `$` could use a different verbosity setting. Additionally, calling `silence` multiple
// times concurrently could change the setting incorrectly.
// However, our pattern of awaiting for both `$` and `silence` calls without any random
// callbacks using either means this interleaving should not ever happen.
export async function silence(
	cmd: ($hh: Shell) => Promise<ProcessOutput>,
): Promise<ProcessOutput> {
	const verboseSetting = $.verbose;
	$.verbose = false;
	const proc = await cmd($);
	$.verbose = verboseSetting;
	return proc;
}
