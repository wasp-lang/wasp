import { type Plugin } from 'vite'
import { spawn } from 'node:child_process'

interface TypeScriptCheckOptions {
  tsConfigPath: string
}

export function typescriptCheck(options: TypeScriptCheckOptions): Plugin {
  return {
    name: 'wasp:typescript-check',
    apply: 'build',
    async buildStart() {
      await runTsc(options.tsConfigPath)
    },
  }
}

function runTsc(tsConfigPath: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const child = spawn(
      'tsc',
      ['--project', tsConfigPath, '--noEmit'],
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
