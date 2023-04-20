import { Command } from 'commander';
import { ensureWaspDirLooksRight } from '../shared/hooks.js';
import { deploy as deployFn } from './deploy/deploy.js';
import {
    ensureDirsInCmdAreAbsoluteAndPresent,
    ensureRailwayReady,
} from './helpers/helpers.js';

class RailwayCommand extends Command {}

const rwDeployCommand = makeRailwayDeployCommand();

function makeRailwayDeployCommand(): RailwayCommand {
    return new RailwayCommand('deploy')
        .description('(Re-)Deploy Wasp app to Railway')
        .option('--skip-build', 'do not run `wasp build` before deploying')
        .option('--skip-client', 'do not deploy the web client')
        .option('--skip-server', 'do not deploy the server')
        .action(deployFn);
}

export function addRailwayCommand(program: Command): void {
    const rw = program
        .command('railway')
        .description('Create and deploy Wasp apps on Railway')
        .addCommand(rwDeployCommand)
        .allowUnknownOption();

    // Add hooks to all commands.
    // Add these hooks before any command-specific ones so they run first.
    rw.commands.forEach((cmd) => {
        cmd.hook('preAction', ensureRailwayReady)
            .hook('preAction', ensureDirsInCmdAreAbsoluteAndPresent)
            .hook('preAction', ensureWaspDirLooksRight);
    });
}
