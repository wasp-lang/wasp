import { $, echo, cd } from 'zx'
import { buildDirExists, cdToClientDir, cdToServerDir, lazyInit } from '../helpers/helpers.js'
import * as tomlHelpers from '../helpers/tomlFileHelpers.js'
import { ICmdOptions, SERVER_CONTEXT_OPTION, CLIENT_CONTEXT_OPTION } from './ICmdOptions.js'

// Runs a command by copying down the toml files, executing it, and copying it back up (just in case).
// If the toml file does not exist, some commands will not run with additional args.
export async function cmd(flyctlArgs: [string], options: ICmdOptions) {
  echo`Running ${options.context} command: flyctl ${flyctlArgs.join(' ')}`

  const buildWaspIfMissing = lazyInit(async () => {
    if (!buildDirExists(options.waspDir)) {
      cd(options.waspDir)
      await $`wasp build`
    }
  })

  if (options.context === SERVER_CONTEXT_OPTION) {
    await buildWaspIfMissing()
    await runServerFlyctlCommand(flyctlArgs, options)
  }

  if (options.context === CLIENT_CONTEXT_OPTION) {
    await buildWaspIfMissing()
    await runClientFlyctlCommand(flyctlArgs, options)
  }
}

async function runServerFlyctlCommand(flyctlArgs: [string], options: ICmdOptions) {
  const tomlFiles = tomlHelpers.getTomlFileInfo(options)

  cdToServerDir(options.waspDir)
  tomlHelpers.deleteLocalToml()
  if (tomlHelpers.serverTomlExists(tomlFiles)) {
    tomlHelpers.copyServerTomlLocally(tomlFiles)
  }

  await runFlyctlCommand(flyctlArgs)

  if (tomlHelpers.localTomlExists()) {
    tomlHelpers.copyLocalTomlAsServerToml(tomlFiles)
  }
}

async function runClientFlyctlCommand(flyctlArgs: [string], options: ICmdOptions) {
  const tomlFiles = tomlHelpers.getTomlFileInfo(options)

  cdToClientDir(options.waspDir)
  tomlHelpers.deleteLocalToml()
  if (tomlHelpers.serverTomlExists(tomlFiles)) {
    tomlHelpers.copyClientTomlLocally(tomlFiles)
  }

  await runFlyctlCommand(flyctlArgs)

  if (tomlHelpers.localTomlExists()) {
    tomlHelpers.copyLocalTomlAsClientToml(tomlFiles)
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
