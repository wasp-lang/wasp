import { Command } from 'commander';
import { ensureWaspDirLooksRight } from '../shared/hooks.js';
import {
    ensureDirsInCmdAreAbsoluteAndPresent,
    ensureRailwayReady,
} from './helpers/helpers.js';

export function addRailwayCommand(program: Command): void {
    const rw = program
        .command('railway')
        .description('Create and deploy Wasp apps on Railway')
        .allowUnknownOption();

    // Add hooks to all commands.
    // Add these hooks before any command-specific ones so they run first.
    rw.commands.forEach((cmd) => {
        cmd.hook('preAction', ensureRailwayReady)
            .hook('preAction', ensureDirsInCmdAreAbsoluteAndPresent)
            .hook('preAction', ensureWaspDirLooksRight);
    });
}
