import * as z from "zod";

import { colorize } from "wasp/universal/ansiColors";

// PRIVATE API (SDK)
export function ensureEnvSchema<Schema extends z.ZodTypeAny>(
  data: unknown,
  schema: Schema,
): z.infer<Schema> {
  const result = getValidatedEnvOrError(data, schema);
  if (result.success) {
    return result.data;
  } else {
    const errorMessage = formatZodEnvErrors(result.error.issues);
    if (globalThis.window) {
      console.error(errorMessage);
    } else {
      console.error(colorize("red", errorMessage));
    }
    throw new Error("Error parsing environment variables");
  }
}

// PRIVATE API (SDK, Vite config)
export function getValidatedEnvOrError<Schema extends z.ZodTypeAny>(
  env: unknown,
  schema: Schema,
): z.SafeParseReturnType<unknown, z.infer<Schema>> {
  return schema.safeParse(env);
}

// PRIVATE API (SDK, Vite config)
export function formatZodEnvErrors(issues: z.ZodIssue[]): string {
  const errorOutput = ["══ Env vars validation failed ══", ""];
  for (const error of issues) {
    errorOutput.push(`${error.path} - ${error.message}`);
  }
  errorOutput.push("", "════════════════════════════════");
  return errorOutput.join("\n");
}
