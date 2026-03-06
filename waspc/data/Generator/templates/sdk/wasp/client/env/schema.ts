{{={= =}=}}
import * as z from 'zod'
import type { FromRegistry } from 'wasp/types'

{=# userClientEnvSchema.isDefined =}
{=& userClientEnvSchema.importStatement =}
const userClientEnvSchema: typeof {= userClientEnvSchema.importIdentifier =} = {= userClientEnvSchema.importIdentifier =}
{=/ userClientEnvSchema.isDefined =}
{=^ userClientEnvSchema.isDefined =}
const userClientEnvSchema = z.object({})
{=/ userClientEnvSchema.isDefined =}

type UserClientEnvSchema = FromRegistry<'clientEnvSchema', z.ZodObject<{}>>

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
