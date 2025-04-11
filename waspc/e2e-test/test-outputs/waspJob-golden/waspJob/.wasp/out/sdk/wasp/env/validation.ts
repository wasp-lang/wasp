import * as z from 'zod'

const redColor = '\x1b[31m'

// PRIVATE API (SDK)
export function ensureEnvSchema<Schema extends z.ZodTypeAny>(
  data: unknown,
  schema: Schema
): z.infer<Schema> {
  const result = getValidatedEnvOrError(data, schema)
  if (result.success) {
    return result.data
  } else {
    console.error(`${redColor}${formatZodEnvErrors(result.error.issues)}`)
    throw new Error('Error parsing environment variables')
  }
}

// PRIVATE API (SDK, Vite config)
export function getValidatedEnvOrError<Schema extends z.ZodTypeAny>(
  env: unknown,
  schema: Schema
): z.SafeParseReturnType<unknown, z.infer<Schema>> {
  return schema.safeParse(env)
}

// PRIVATE API (SDK, Vite config)
export function formatZodEnvErrors(issues: z.ZodIssue[]): string {
  const errorOutput = ['', '══ Env vars validation failed ══', '']
  for (const error of issues) {
    errorOutput.push(` - ${error.message}`)
  }
  errorOutput.push('')
  errorOutput.push('════════════════════════════════')
  return errorOutput.join('\n')
}
