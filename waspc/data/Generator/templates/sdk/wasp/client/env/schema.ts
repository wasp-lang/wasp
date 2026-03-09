{{={= =}=}}
import * as z from 'zod'

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
export const userClientEnvSchema = {= envValidationSchema.importIdentifier =}
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
export const userClientEnvSchema = null
{=/ envValidationSchema.isDefined =}

const serverUrlSchema = z
  .string({
    error: '{= serverUrlEnvVarName =} is required',
  })
  .url({
    error: '{= serverUrlEnvVarName =} must be a valid URL',
  })

const waspClientDevSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema
    .default('{= defaultServerUrl =}'),
})

const waspClientProdSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema,
})

// PRIVATE API (sdk, Vite config)
export function getClientWaspEnvSchema(mode: string) {
  return mode === 'production' ? waspClientProdSchema : waspClientDevSchema
}
