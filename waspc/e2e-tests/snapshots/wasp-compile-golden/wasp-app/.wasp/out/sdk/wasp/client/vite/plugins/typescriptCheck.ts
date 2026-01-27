import { type Plugin } from 'vite'
import { execSync } from 'child_process'

export function typescriptCheck(): Plugin {
  return {
    name: 'wasp:typescript-check',
    apply: 'build',
    buildStart() {
      // Inherit stdio to display TypeScript errors in the terminal.
      execSync('tsc --build --noEmit', { stdio: 'inherit' })
    },
  }
}
