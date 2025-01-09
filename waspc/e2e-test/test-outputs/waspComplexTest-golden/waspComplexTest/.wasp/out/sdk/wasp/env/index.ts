import * as z from 'zod'

// PUBLIC API
export function defineEnvValidationSchema<Schema extends z.ZodObject<any>>(
  schema: Schema,
): Schema {
  return schema
}

// PRIVATE API (SDK, Vite config)
export { ensureEnvSchema } from './validation.js'
