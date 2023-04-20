import { Command, Option } from 'commander';
import { ensureRailwayReady } from './helpers/helpers.js';

export function addRailwayCommand(program: Command): void {
    const fly = program
        .command('railway')
        .description('Create and deploy Wasp apps on Railway')
        .allowUnknownOption();

    // Add global options and hooks to all commands.
    // Add these hooks before any command-specific ones so they run first.
    // NOTE: When we add another provider, consider pulling `--wasp-exe` and `--wasp-project-dir`
    // up as a global option that every provider can use (if possible).
    fly.commands.forEach((cmd) => {
        cmd.addOption(
            new Option(
                '--wasp-exe <path>',
                'Wasp executable (either on PATH or absolute path)',
            )
                .hideHelp()
                .makeOptionMandatory(),
        )
            .addOption(
                new Option(
                    '--wasp-project-dir <dir>',
                    'absolute path to Wasp project dir',
                )
                    .hideHelp()
                    .makeOptionMandatory(),
            )
            .hook('preAction', ensureRailwayReady);
        /* .hook("preAction", ensureDirsInCmdAreAbsoluteAndPresent) */
        /* .hook("preAction", ensureWaspDirLooksRight); */
    });
}
