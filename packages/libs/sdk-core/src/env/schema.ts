import * as z from "zod";

export function defineEnvValidationSchema<Schema extends z.ZodObject>(
  schema: Schema,
): Schema {
  return schema;
}
