import { type Plugin } from 'vite'
import { spawn } from 'node:child_process'

interface TypeScriptCheckOptions {
  srcTsConfigPath: string
}

export function typescriptCheck(options: TypeScriptCheckOptions): Plugin {
  return {
    name: 'wasp:typescript-check',
    apply: 'build',
    // `buildStart` runs once per environment, and a build has several of them
    // (`client`, `ssr` and Nitro's). Type checking the user's source doesn't
    // depend on the environment, so we only do it in one of them.
    applyToEnvironment: (environment) => environment.name === 'client',
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
