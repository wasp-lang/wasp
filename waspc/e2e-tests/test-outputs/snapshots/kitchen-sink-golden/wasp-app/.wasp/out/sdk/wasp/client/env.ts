import { getClientEnvSchema } from './env/schema.js'
import { ensureEnvSchema } from '../env/validation.js'

// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, getClientEnvSchema(import.meta.env.MODE))
