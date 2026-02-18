// @ts-nocheck
{{={= =}=}}
import { registerClientEnvSchema } from 'wasp/client/env/envRegistry'
{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
registerClientEnvSchema({= envValidationSchema.importIdentifier =})
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
import * as z from 'zod'
registerClientEnvSchema(z.object({}))
{=/ envValidationSchema.isDefined =}
