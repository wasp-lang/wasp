import { type Plugin } from 'vite'
import { spawn } from 'node:child_process'
import { ENVIRONMENT_NAMES } from '../../../vite/constants.js'

interface TypeScriptCheckOptions {
  srcTsConfigPath: string
}

export function typescriptCheck(options: TypeScriptCheckOptions): Plugin {
  return {
    name: 'wasp:typescript-check',
    apply: 'build',
    // `buildStart` runs once per environment, but the type check covers the
    // whole project, so we only run it in the client environment.
    applyToEnvironment: (environment) =>
      environment.name === ENVIRONMENT_NAMES.CLIENT,
    async buildStart() {
      await runTsc(options.srcTsConfigPath)
    },
  }
}

function runTsc(srcTsConfigPath: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const child = spawn(
      'tsc',
      ['--project', srcTsConfigPath, '--noEmit'],
      {
        stdio: 'inherit',
        shell: process.platform === 'win32',
      }
    )

    child.once('error', reject)
    child.once('close', (code) =>
      code === 0
        ? resolve()
        : reject(new Error(`TypeScript check failed (exit ${code})`))
    )
  })
}
