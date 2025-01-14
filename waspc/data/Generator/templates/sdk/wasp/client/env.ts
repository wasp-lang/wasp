{{={= =}=}}
import * as z from 'zod'

import { clientEnvSchema as waspClientEnvSchema } from './env/schema.js'
import { ensureEnvSchema } from '../env/validation.js'

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
const userClientEnvSchema = {= envValidationSchema.importIdentifier =}
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
const userClientEnvSchema = z.object({})
{=/ envValidationSchema.isDefined =}

const clientEnvSchema = userClientEnvSchema.merge(waspClientEnvSchema)

// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)
