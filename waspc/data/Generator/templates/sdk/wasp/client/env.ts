{{={= =}=}}
import * as z from 'zod'

import { ensureEnvSchema } from '../env/index.js'

{=# envValidationFn.isDefined =}
{=& envValidationFn.importStatement =}
const userClientEnvSchema = {= envValidationFn.importIdentifier =}()
{=/ envValidationFn.isDefined =}
{=^ envValidationFn.isDefined =}
const userClientEnvSchema = z.object({})
{=/ envValidationFn.isDefined =}

const waspClientEnvSchema = z.object({
  REACT_APP_API_URL: z
    .string({
      required_error: 'REACT_APP_API_URL is required',
    })
    .default('{= defaultServerUrl =}')
})

const clientEnvSchema = waspClientEnvSchema.merge(userClientEnvSchema)

export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)
