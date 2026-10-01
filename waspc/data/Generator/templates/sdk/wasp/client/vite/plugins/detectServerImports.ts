{{={= =}=}}
import { detectServerImports as coreDetectServerImports } from '@wasp.sh/lib-sdk-core/node/vite'

export function detectServerImports() {
  return coreDetectServerImports('{= srcDirInWaspProjectDir =}')
}
