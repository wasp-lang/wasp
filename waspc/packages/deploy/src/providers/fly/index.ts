import { Command, Option } from 'commander'
import { setup as setupFn } from './setup/setup.js'
import { deploy as deployFn } from './deploy/deploy.js'
import { createDb as createDbFn } from './createDb/createDb.js'
import { cmd as cmdFn } from './cmd/cmd.js'
import { ensureWaspDirLooksRight, ensureDirsAreAbsolute } from './helpers/helpers.js'
import { ensureFlyReady, ensureRegionIsValid } from './helpers/flyctlHelpers.js'
import { CLIENT_CONTEXT_OPTION, SERVER_CONTEXT_OPTION } from './cmd/CmdOptions.js'

export const flySetupCommand = makeFlySetupCommand()
export const createFlyDbCommand = makeCreateFlyDbCommand()
export const flyDeployCommand = makeFlyDeployCommand()
export const executeFlyCommand = makeExecuteFlyCommand()

export function addFlyCommand(program: Command) {
  const fly = program.command('fly')
  fly.description('Setup and deploy Wasp apps on Fly.io')
    .addCommand(flySetupCommand)
    .addCommand(createFlyDbCommand)
    .addCommand(flyDeployCommand)
    .addCommand(executeFlyCommand)
    .allowUnknownOption()

  // Add global options and hooks to all commands.
  // Add these hooks before any command-specific ones so they run first.
  fly.commands.forEach((cmd) => {
    cmd.requiredOption('--wasp-dir <dir>', 'path to Wasp project')
      .option('--toml-dir <dir>', 'path to where fly.toml files should live')
      .hook('preAction', ensureFlyReady)
      .hook('preAction', ensureDirsAreAbsolute)
      .hook('preAction', ensureWaspDirLooksRight)
  })

  // Add command-specific hooks.
  flySetupCommand.hook('preAction', (_thisCommand, actionCommand) => ensureRegionIsValid(actionCommand.args[1]))
  createFlyDbCommand.hook('preAction', (_thisCommand, actionCommand) => ensureRegionIsValid(actionCommand.args[0]))
}

function makeFlySetupCommand(): Command {
  const setup = new Command('setup')
  setup.description('Set up a new app on Fly.io (this does not deploy it)')
    .argument('<basename>', 'base app name to use on Fly.io (must be unique)')
    .argument('<region>', 'deployment region to use on Fly.io')
    .action(setupFn)
  return setup
}

function makeFlyDeployCommand(): Command {
  const deploy = new Command('deploy')
  deploy.description('(Re-)Deploy existing app to Fly.io')
    .option('--skip-build', 'do not run `wasp build` before deploying')
    .action(deployFn)
  return deploy
}

function makeExecuteFlyCommand(): Command {
  const cmd = new Command('cmd')
  const context = new Option('--context <context>', 'client or server context')
    .choices([SERVER_CONTEXT_OPTION, CLIENT_CONTEXT_OPTION])
    .makeOptionMandatory()
  cmd.description('Run arbitrary flyctl commands for server or client')
    .argument('<cmd...>', 'flyctl command to run in server/client context')
    .addOption(context)
    .action(cmdFn)
    .allowUnknownOption()
  return cmd
}

function makeCreateFlyDbCommand(): Command {
  const createDb = new Command('create-db')
  createDb.description('Creates a Postgres DB and attaches it to the server app')
    .argument('<region>', 'deployment region to use on Fly.io')
    .option('--vm-size <vmSize>', 'flyctl postgres create option', 'shared-cpu-1x')
    .option('--initial-cluster-size <initialClusterSize>', 'flyctl postgres create option', '1')
    .option('--volume-size <volumeSize>', 'flyctl postgres create option', '1')
    .action(createDbFn)
  return createDb
}
