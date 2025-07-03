import { clientEnvSchema } from './env/schema.js'
import { ensureEnvSchema } from '../env/validation.js'

// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)
