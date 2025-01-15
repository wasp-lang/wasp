{{={= =}=}}
import * as z from 'zod'

import { ensureEnvSchema } from '../env/validation.js'

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
const userClientEnvSchema = {= envValidationSchema.importIdentifier =}
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
const userClientEnvSchema = z.object({})
{=/ envValidationSchema.isDefined =}

const waspClientEnvSchema = z.object({
  REACT_APP_API_URL: z
    .string({
      required_error: 'REACT_APP_API_URL is required',
    })
    .default('{= defaultServerUrl =}')
})

const clientEnvSchema = userClientEnvSchema.merge(waspClientEnvSchema)

// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)
