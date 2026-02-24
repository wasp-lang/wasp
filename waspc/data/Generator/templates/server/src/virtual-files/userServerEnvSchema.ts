// @ts-nocheck
{{={= =}=}}
{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
export const userServerEnvSchema = {= envValidationSchema.importIdentifier =}
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
import * as z from 'zod'
export const userServerEnvSchema = z.object({})
{=/ envValidationSchema.isDefined =}
