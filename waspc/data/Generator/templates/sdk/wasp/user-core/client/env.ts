import { ensureEnvSchema } from 'wasp/env/validation'
import { clientEnvSchema } from './env/schema.js'

// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)
