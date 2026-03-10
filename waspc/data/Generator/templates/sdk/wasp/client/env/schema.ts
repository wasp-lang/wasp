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

const serverUrlSchema = z
  .string({
    required_error: '{= serverUrlEnvVarName =} is required',
  })
  .url({
    message: '{= serverUrlEnvVarName =} must be a valid URL',
  })

const waspClientDevSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema
    .default('{= defaultServerUrl =}'),
})

const waspClientProdSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema,
})

// PRIVATE API (sdk, Vite config)
export function getClientEnvSchema(mode: string) {
  const waspSchema = mode === 'production' ? waspClientProdSchema : waspClientDevSchema
  return userClientEnvSchema.merge(waspSchema)
}
