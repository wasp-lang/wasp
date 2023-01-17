import { Command } from 'commander'
import { exit } from 'process'
import { cd } from 'zx'
import fs from 'fs'
import path from 'node:path'
import chalk from 'chalk'

export function isYes(str: string): boolean {
  return str.trim().toLowerCase().startsWith('y')
}

export function ensureWaspDirLooksRight(thisCommand: Command) {
  if (!fs.existsSync(path.join(thisCommand.opts().waspDir, '.wasproot'))) {
    waspSays(`The supplied Wasp directory does not appear to be a valid Wasp project.`)
    waspSays(`Please double check your path.`)
    exit(1)
  }
}

export function buildDirExists(waspDir: string): boolean {
  return fs.existsSync(path.join(waspDir, '.wasp', 'build'))
}

export function cdToServerBuildDir(waspDir: string) {
  cd(path.join(waspDir, '.wasp', 'build'))
}

export function cdToClientBuildDir(waspDir: string) {
  cd(path.join(waspDir, '.wasp', 'build', 'web-app'))
}

export function ensureDirsAreAbsolute(thisCommand: Command) {
  if (thisCommand.opts().waspDir && !path.isAbsolute(thisCommand.opts().waspDir)) {
    waspSays(`The Wasp dir path must be absolute.`)
    exit(1)
  }

  if (thisCommand.opts().tomlDir && !path.isAbsolute(thisCommand.opts().tomlDir)) {
    waspSays(`The toml dir path must be absolute.`)
    exit(1)
  }
}

// Promises are eager and start running right when created.
// This lets us create a promise that will won't execute right away.
// Additionally, like a normal promise, it can still be awaited many times
// but only runs to completion once.
export function lazyInit<Type>(fn: () => Promise<Type>) {
  let prom: Promise<Type> | undefined = undefined
  return () => prom = (prom || fn())
}

export function waspSays(str: string) {
  console.log("🐝 " + chalk.yellow(str))
}
