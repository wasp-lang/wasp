{{={= =}=}}
import * as z from 'zod'

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
const userClientEnvSchema = {= envValidationSchema.importIdentifier =}
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
const userClientEnvSchema = z.object({})
{=/ envValidationSchema.isDefined =}

const serverUrlSchema =
  z.string({
    error: '{= serverUrlEnvVarName =} is required',
  })
  .pipe(
    z.url({
      error: '{= serverUrlEnvVarName =} must be a valid URL',
    })
  )

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
  return z.object({ ...userClientEnvSchema.shape, ...waspSchema.shape })
}
