import type { StandardSchemaV1 } from '@standard-schema/spec'

// PUBLIC API
export function defineEnvValidationSchema<Schema extends StandardSchemaV1>(
  schema: Schema,
): Schema {
  return schema
}
