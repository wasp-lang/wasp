import * as z from "zod";

import { colorize } from "wasp/universal/ansiColors";

// PRIVATE API (SDK)
export function ensureEnvSchema<Schema extends z.ZodType>(
  data: unknown,
  schema: Schema,
): z.infer<Schema> {
  const result = getValidatedEnvOrError(data, schema);
  if (result.success) {
    return result.data;
  } else {
    const errorMessage = formatZodEnvError(result.error);
    console.error(colorize("red", errorMessage));
    throw new Error("Error parsing environment variables");
  }
}

// PRIVATE API (SDK, Vite config)
export function getValidatedEnvOrError<Schema extends z.ZodType>(
  env: unknown,
  schema: Schema,
): z.ZodSafeParseResult<z.infer<Schema>> {
  return schema.safeParse(env);
}

// PRIVATE API (SDK, Vite config)
export function formatZodEnvError(error: z.ZodError): string {
  const flattenedIssues = z.flattenError(error);

  return [
    "══ Env vars validation failed ══",
    "",
    // Top-level errors
    ...flattenedIssues.formErrors,
    "",
    // Errors per field
    ...Object.entries(flattenedIssues.fieldErrors).map(
      ([prop, error]) => `${prop} - ${error}`,
    ),
    "",
    "════════════════════════════════",
  ].join("\n");
}
