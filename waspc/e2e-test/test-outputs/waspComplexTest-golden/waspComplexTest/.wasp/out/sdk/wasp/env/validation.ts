import * as z from 'zod'

const redColor = '\x1b[31m'

// PRIVATE API (SDK, Vite config)
export function ensureEnvSchema<Schema extends z.ZodTypeAny>(
  data: unknown,
  schema: Schema
): z.infer<Schema> {
  const result = getValidatedEnvOrError(data, schema)
  switch (result.type) {
    case 'error':
      console.error(`${redColor}${result.message}`)
      throw new Error('Error parsing environment variables')
    case 'success':
      return result.env
    default:
      result satisfies never;
  }
}

// PRIVATE API (Vite config)
export type EnvValidationResult<Env> = {
  type: 'error',
  message: string,
} | {
  type: 'success',
  env: Env,
}

// PRIVATE API (SDK, Vite config)
export function getValidatedEnvOrError<Schema extends z.ZodTypeAny>(
  env: unknown,
  schema: Schema
): EnvValidationResult<z.infer<Schema>> {
  try {
    const validatedEnv = schema.parse(env)
    return {
      type: 'success',
      env: validatedEnv,
    }
  } catch (e) {
    if (e instanceof z.ZodError) {
      const errorOutput = ['', '══ Env vars validation failed ══', '']
      for (const error of e.errors) {
        errorOutput.push(` - ${error.message}`)
      }
      errorOutput.push('')
      errorOutput.push('════════════════════════════════')
      return {
        type: 'error',
        message: errorOutput.join('\n'),
      }
    } else {
      return {
        type: 'error',
        message: e.message,
      }
    }
  }
}
