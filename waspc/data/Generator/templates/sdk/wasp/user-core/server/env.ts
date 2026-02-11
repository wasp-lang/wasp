{{={= =}=}}
import * as z from 'zod'

import { ensureEnvSchema } from '../../core/env/validation.js'
import { serverDevEnvSchema, serverProdEnvSchema, unvalidatedEnv, commonServerEnvSchema } from '../../core/server/waspEnv.js'

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
const userServerEnvSchema = {= envValidationSchema.importIdentifier =}
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
const userServerEnvSchema = z.object({})
{=/ envValidationSchema.isDefined =}

const fullServerEnvSchema = userServerEnvSchema.merge(commonServerEnvSchema)
const serverEnvSchema = z.discriminatedUnion('NODE_ENV', [
  serverDevEnvSchema.merge(fullServerEnvSchema),
  serverProdEnvSchema.merge(fullServerEnvSchema)
])

// PUBLIC API
export const env = ensureEnvSchema(
  unvalidatedEnv,
  serverEnvSchema,
)
