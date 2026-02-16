{{={= =}=}}
import * as z from 'zod'
import { getClientEnvSchema } from './envRegistry.js'
import type { Register } from 'wasp/types'

type UserClientEnvSchema =
  Register extends { clientEnvSchema: infer T extends z.ZodTypeAny }
    ? T
    : z.ZodObject<{}>

const userClientEnvSchema = getClientEnvSchema() as UserClientEnvSchema

const waspClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": z
  .string()
  .url({
    message: '{= serverUrlEnvVarName =} must be a valid URL',
  })
  .default('{= defaultServerUrl =}'),
})

// PRIVATE API (sdk, Vite config)
export const clientEnvSchema = userClientEnvSchema.merge(waspClientEnvSchema)
