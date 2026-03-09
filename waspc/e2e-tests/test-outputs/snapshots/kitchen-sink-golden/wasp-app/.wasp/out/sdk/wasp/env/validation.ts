import * as z from 'zod'
import type { StandardSchemaV1 } from '@standard-schema/spec'

import { getColorizedConsoleFormatString } from 'wasp/universal/ansiColors'

const redColorFormatString = getColorizedConsoleFormatString('red');

// PRIVATE API (SDK)
export function ensureZodEnvSchema<Schema extends z.ZodType>(
  data: unknown,
  schema: Schema
): z.infer<Schema> {
  const result = getValidatedZodEnvOrError(data, schema)
  if (result.success) {
    return result.data
  } else {
    console.error(`${redColorFormatString}${formatZodEnvErrors(result.error.issues)}`)
    throw new Error('Error parsing environment variables')
  }
}

// PRIVATE API (SDK)
export function ensureStandardSchemaEnv<Schema extends StandardSchemaV1>(
  data: unknown,
  schema: Schema
): StandardSchemaV1.InferOutput<Schema> {
  const result = getValidatedStandardSchemaEnvOrError(data, schema)
  if (!result.issues) {
    return result.value as StandardSchemaV1.InferOutput<Schema>
  } else {
    console.error(`${redColorFormatString}${formatStandardSchemaErrors(result.issues)}`)
    throw new Error('Error parsing environment variables')
  }
}

// PRIVATE API (SDK, Vite config)
export function getValidatedZodEnvOrError<Schema extends z.ZodType>(
  env: unknown,
  schema: Schema
): z.ZodSafeParseResult<z.infer<Schema>> {
  return schema.safeParse(env)
}

// PRIVATE API (SDK, Vite config)
export function getValidatedStandardSchemaEnvOrError<Schema extends StandardSchemaV1>(
  env: unknown,
  schema: Schema
): StandardSchemaV1.Result<StandardSchemaV1.InferOutput<Schema>> {
  const result = schema['~standard'].validate(env)
  if (result instanceof Promise) {
    throw new Error(
      'Async schema validation is not supported for environment variables. ' +
      'Please use a schema library that supports synchronous validation.'
    )
  }
  return result as StandardSchemaV1.Result<StandardSchemaV1.InferOutput<Schema>>
}

// PRIVATE API (SDK, Vite config)
export function formatZodEnvErrors(issues: z.core.$ZodIssue[]): string {
  const errorOutput = ['', '══ Env vars validation failed ══', '']
  for (const error of issues) {
    errorOutput.push(` - ${error.message}`)
  }
  errorOutput.push('')
  errorOutput.push('════════════════════════════════')
  return errorOutput.join('\n')
}

// PRIVATE API (SDK, Vite config)
export function formatStandardSchemaErrors(issues: ReadonlyArray<StandardSchemaV1.Issue>): string {
  const errorOutput = ['', '══ Env vars validation failed ══', '']
  for (const issue of issues) {
    const path = issue.path
      ?.map((segment) => (typeof segment === 'object' ? segment.key : segment))
      .join('.')
    const prefix = path ? `${path}: ` : ''
    errorOutput.push(` - ${prefix}${issue.message}`)
  }
  errorOutput.push('')
  errorOutput.push('════════════════════════════════')
  return errorOutput.join('\n')
}
