import { exit } from 'process';
import { LaunchOptions } from './LaunchOptions.js';
import { DeployOptions } from '../deploy/DeployOptions.js';
import { getCommandHelp, waspSays } from '../helpers/helpers.js';
import { setup } from '../setup/setup.js';
import { createDb } from '../createDb/createDb.js';
import { deploy } from '../deploy/deploy.js';
import {
  clientTomlExistsInProject,
  getTomlFilePaths,
  serverTomlExistsInProject,
} from '../helpers/tomlFileHelpers.js';
import { createFlyDbCommand, flyDeployCommand, flySetupCommand } from '../index.js';

export async function launch(
  basename: string,
  region: string,
  options: LaunchOptions,
): Promise<void> {
  waspSays('Launching your Wasp app to Fly.io!');

  const tomlFilePaths = getTomlFilePaths(options);
  if (serverTomlExistsInProject(tomlFilePaths) || clientTomlExistsInProject(tomlFilePaths)) {
    waspSays(
      'You already have Fly toml files. The launch command is intended to be run one time on a new Fly project. Please try a different command.',
    );
    exit(1);
  }

  try {
    await setup(basename, region, options);
  } catch (e) {
    waspSays(
      `There was an error running "${getCommandHelp(
        flySetupCommand,
      )}". Please review the error and try again (if appropriate).`,
    );
    exit(1);
  }

  try {
    await createDb(region, options);
  } catch (e) {
    waspSays(
      `There was an error running "${getCommandHelp(
        createFlyDbCommand,
      )}". Please review the error and try again (if appropriate).`,
    );
    exit(1);
  }

  try {
    const deployOptions: DeployOptions = { ...options, skipBuild: true };
    await deploy(deployOptions);
  } catch (e) {
    waspSays(
      `There was an error running "${getCommandHelp(
        flyDeployCommand,
      )}". Please review the error and try again (if appropriate).`,
    );
    exit(1);
  }
}
