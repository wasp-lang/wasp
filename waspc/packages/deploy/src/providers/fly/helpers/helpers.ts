import { Command } from 'commander';
import fs from 'fs';
import {
    ensureDirAbsoluteAndExists,
    getWaspBuildDir,
} from '../../shared/helpers.js';

export function buildDirExists(waspProjectDir: string): boolean {
    const waspBuildDir = getWaspBuildDir(waspProjectDir);
    return fs.existsSync(waspBuildDir);
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
