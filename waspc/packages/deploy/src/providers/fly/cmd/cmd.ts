import { $, cd } from 'zx'
import { ClientCommonOps, ICommonOps, ServerCommonOps } from '../helpers/ICommonOps.js'
import { buildDirExists, waspSays } from '../helpers/helpers.js'
import * as tomlHelpers from '../helpers/tomlFileHelpers.js'
import { ICmdOptions, SERVER_CONTEXT_OPTION } from './ICmdOptions.js'

// Runs a command by copying down the project toml files, executing it, and copying it back up (just in case).
// If the toml file does not exist, some commands will not run with additional args (e.g. -a <appname>).
export async function cmd(flyctlArgs: [string], options: ICmdOptions) {
  waspSays(`Running ${options.context} command: flyctl ${flyctlArgs.join(' ')}`)

  if (!buildDirExists(options.waspDir)) {
    cd(options.waspDir)
    await $`wasp build`
  }

  const tomlFiles = tomlHelpers.getTomlFileInfo(options)
  let commonOps: ICommonOps

  if (options.context === SERVER_CONTEXT_OPTION) {
    commonOps = new ServerCommonOps(options.waspDir, tomlFiles)
  } else {
    commonOps = new ClientCommonOps(options.waspDir, tomlFiles)
  }

  await runFlyctlCommand(commonOps, flyctlArgs)
}

async function runFlyctlCommand(commonOps: ICommonOps, flyctlArgs: [string]) {
  commonOps.cdToBuildDir()
  tomlHelpers.deleteLocalToml()
  if (commonOps.tomlExistsInProject()) {
    commonOps.copyProjectTomlLocally()
  }

  try {
    await $`flyctl ${flyctlArgs}`
  } catch {
    waspSays(`Error running command. Note: many commands require a toml file or a -a option specifying the app name.`)
    waspSays(`If you already have an app, consider running "config save -- -a <app-name>".`)
  }

  if (tomlHelpers.localTomlExists()) {
    commonOps.copyLocalTomlToProject()
  }
}
