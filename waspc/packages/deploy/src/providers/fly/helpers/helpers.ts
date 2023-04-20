import { Command } from 'commander';
import { exit } from 'process';
import { cd } from 'zx';
import fs from 'fs';
import path from 'node:path';
import { waspSays } from '../../shared/helpers.js';

export function ensureWaspDirLooksRight(thisCommand: Command): void {
	const dirContainsWasproot = fs.existsSync(path.join(thisCommand.opts().waspProjectDir, '.wasproot'));
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

export function ensureDirsInCmdAreAbsoluteAndPresent(thisCommand: Command): void {
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
