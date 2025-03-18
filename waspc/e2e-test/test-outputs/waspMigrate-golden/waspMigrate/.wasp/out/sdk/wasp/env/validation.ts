import * as z from 'zod'
import type { Result } from '../universal/types'

const redColor = '\x1b[31m'

// PRIVATE API (SDK)
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
      return result.data
    default:
      result satisfies never;
  }
}

// PRIVATE API (SDK, Vite config)
export function getValidatedEnvOrError<Schema extends z.ZodTypeAny>(
  env: unknown,
  schema: Schema
): Result<z.infer<Schema>> {
  try {
    const validatedEnv = schema.parse(env)
    return {
      type: 'success',
      data: validatedEnv,
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
