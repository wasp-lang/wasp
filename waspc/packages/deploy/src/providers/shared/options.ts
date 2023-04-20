import { Command, Option } from 'commander';

export const createGlobalOptions = (cmd: Command): void => {
    cmd.addOption(
        new Option(
            '--wasp-exe <path>',
            'Wasp executable (either on PATH or absolute path)',
        )
            .hideHelp()
            .makeOptionMandatory(),
    ).addOption(
        new Option(
            '--wasp-project-dir <dir>',
            'absolute path to Wasp project dir',
        )
            .hideHelp()
            .makeOptionMandatory(),
    );
};
