import { getClientWaspEnvSchema, userClientEnvSchema } from './env/schema.js'
import { ensureZodEnvSchema, ensureStandardSchemaEnv } from '../env/validation.js'

const waspEnv = ensureZodEnvSchema(import.meta.env, getClientWaspEnvSchema(import.meta.env.MODE))

const userEnv = userClientEnvSchema
  ? ensureStandardSchemaEnv(import.meta.env, userClientEnvSchema)
  : {}

// PUBLIC API
export const env = { ...waspEnv, ...userEnv }
