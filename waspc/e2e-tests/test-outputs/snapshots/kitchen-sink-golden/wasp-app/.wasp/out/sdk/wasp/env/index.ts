import * as z from 'zod'

// PUBLIC API
export function defineEnvValidationSchema<Schema extends z.ZodObject>(
  schema: Schema,
): Schema {
  return schema
}

