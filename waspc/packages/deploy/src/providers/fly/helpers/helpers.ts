import { Command } from 'commander';
import { cd } from 'zx';
import fs from 'fs';
import path from 'node:path';
import { ensureDirAbsoluteAndExists } from '../../shared/helpers.js';

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
    const waspProjectDirPath: string | undefined = thisCommand.opts()
        .waspProjectDir;
    ensureDirAbsoluteAndExists({ label: 'Wasp dir', dir: waspProjectDirPath });

    const flyTomlDirPath: string | undefined = thisCommand.opts().flyTomlDir;
    ensureDirAbsoluteAndExists({ label: 'toml dir', dir: flyTomlDirPath });
}
