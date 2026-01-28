import { clientEnvSchema } from './env/schema.js'
import { ensureEnvSchema } from 'wasp/env/validation'

// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)
