import { type Plugin } from 'vite'
import { spawn } from 'node:child_process'

export function typescriptCheck(): Plugin {
  return {
    name: 'wasp:typescript-check',
    apply: 'build',
    async buildStart() {
      await runTsc()
    },
  }
}

function runTsc(): Promise<void> {
  return new Promise((resolve, reject) => {
    const child = spawn(
      'tsc',
      ['--build', '--noEmit'],
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
