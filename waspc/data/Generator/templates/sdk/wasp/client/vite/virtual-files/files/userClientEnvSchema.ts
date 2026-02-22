// @ts-nocheck
{{={= =}=}}
{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
export const userClientEnvSchema = {= envValidationSchema.importIdentifier =}
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
import * as z from 'zod'
export const userClientEnvSchema = z.object({})
{=/ envValidationSchema.isDefined =}
