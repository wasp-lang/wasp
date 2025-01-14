import * as z from 'zod'

import { clientEnvSchema as waspClientEnvSchema } from './env/schema.js'
import { ensureEnvSchema } from '../env/validation.js'

const userClientEnvSchema = z.object({})

const clientEnvSchema = userClientEnvSchema.merge(waspClientEnvSchema)

// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)
