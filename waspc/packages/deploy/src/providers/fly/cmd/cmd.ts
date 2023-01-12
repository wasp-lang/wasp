import { $, echo } from 'zx'
import { cdToClientDir, cdToServerDir } from '../helpers/helpers.js'
import * as tomlHelpers from '../helpers/tomlFileHelpers.js'
import { ICmdOptions, SERVER_CONTEXT_OPTION, CLIENT_CONTEXT_OPTION } from './ICmdOptions.js'

// Runs a command by copying down the toml files, executing it, and copying it back up (just in case).
// If the toml file does not exist, some commands will not run with additional args.
export async function cmd(flyctlArgs: [string], options: ICmdOptions) {
  const tomlFiles = tomlHelpers.getTomlFileInfo(options)

  echo`Running ${options.context} command: flyctl ${flyctlArgs.join(' ')}`

  if (options.context === SERVER_CONTEXT_OPTION) {
    cdToServerDir(options.waspDir)
    tomlHelpers.deleteLocalToml()
    if (tomlHelpers.serverTomlExists(tomlFiles)) {
      tomlHelpers.copyServerTomlLocally(tomlFiles)
    }

    runFlyctlCommand(flyctlArgs)

    if (tomlHelpers.localTomlExists()) {
      tomlHelpers.copyLocalTomlAsServerToml(tomlFiles)
    }
  }

  if (options.context === CLIENT_CONTEXT_OPTION) {
    cdToClientDir(options.waspDir)
    tomlHelpers.deleteLocalToml()
    if (tomlHelpers.serverTomlExists(tomlFiles)) {
      tomlHelpers.copyClientTomlLocally(tomlFiles)
    }

    runFlyctlCommand(flyctlArgs)

    if (tomlHelpers.localTomlExists()) {
      tomlHelpers.copyLocalTomlAsClientToml(tomlFiles)
    }
  }
}

async function runFlyctlCommand(flyctlArgs: [string]) {
  try {
    await $`flyctl ${flyctlArgs}`
  } catch {
    echo`Error running command. Note: many commands require a toml file or a -a option specifying the app name.`
    echo`If you already have an app, consider running "config save -- -a <app-name>".`
  }
}
