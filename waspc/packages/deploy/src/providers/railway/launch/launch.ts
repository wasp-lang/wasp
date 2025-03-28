import { exit } from 'process';
import { LaunchOptions } from './LaunchOptions.js';
import { DeployOptions } from '../deploy/DeployOptions.js';
import { setup } from '../setup/setup.js';
import { deploy } from '../deploy/deploy.js';
import { railwayDeployCommand, railwaySetupCommand } from '../index.js';
import { waspSays, getCommandHelp } from '../../../helpers.js';

export async function launch(basename: string, options: LaunchOptions): Promise<void> {
  waspSays('Launching your Wasp app to Railway!');

  try {
    await setup(basename, options);
  } catch (e) {
    waspSays(
      `There was an error running "${getCommandHelp(
        railwaySetupCommand,
      )}". Please review the error and try again (if appropriate).`,
    );
    exit(1);
  }

  try {
    await deploy(basename, { ...options, skipBuild: true });
  } catch (e) {
    waspSays(
      `There was an error running "${getCommandHelp(
        railwayDeployCommand,
      )}". Please review the error and try again (if appropriate).`,
    );
    exit(1);
  }
}
