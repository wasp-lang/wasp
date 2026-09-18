import { validateEnv as coreValidateEnv } from '@wasp.sh/lib-sdk-core/node/vite'

export function validateEnv() {
  return coreValidateEnv('.wasp/out/sdk/wasp/client/env.ts')
}
