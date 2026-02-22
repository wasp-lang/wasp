{{={= =}=}}
import * as z from 'zod'
import { userClientEnvSchema } from '{= userClientEnvSchemaPath =}'
import type { GetConfigFromRegistry } from 'wasp/types'

type UserClientEnvSchema = GetConfigFromRegistry<'clientEnvSchema', z.ZodObject<{}>>

const waspClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": z
  .string()
  .url({
    message: '{= serverUrlEnvVarName =} must be a valid URL',
  })
  .default('{= defaultServerUrl =}'),
})

// PRIVATE API (sdk, Vite config)
export const clientEnvSchema = waspClientEnvSchema.merge(userClientEnvSchema as UserClientEnvSchema)
