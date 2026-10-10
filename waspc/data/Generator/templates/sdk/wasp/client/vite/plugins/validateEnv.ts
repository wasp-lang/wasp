{{={= =}=}}
import { validateEnv as coreValidateEnv } from '@wasp.sh/lib-sdk-core/node/vite'

export function validateEnv() {
  return coreValidateEnv('{= clientEnvSchemaValidationModulePath =}')
}
