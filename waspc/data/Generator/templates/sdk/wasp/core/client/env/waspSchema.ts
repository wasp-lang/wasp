{{={= =}=}}
import * as z from 'zod'

export const waspClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": z
  .string()
  .url({
    message: '{= serverUrlEnvVarName =} must be a valid URL',
  })
  .default('{= defaultServerUrl =}'),
})

