import { type Plugin, loadEnv } from "vite";

import { ensureEnvSchema } from 'wasp/env'
import { clientEnvSchema } from 'wasp/client/env/schema'

export function validateEnv(): Plugin {
  return {
    name: 'wasp-validate-env',
    config: (config) => {
      const env = loadEnv(config.mode, process.cwd(), config.envPrefix)
      ensureEnvSchema(env, clientEnvSchema)
    },
  };
}
