import { ensureEnvSchema } from '../../core/env/validation.js'
import { clientEnvSchema } from './env/schema.js'

// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)
