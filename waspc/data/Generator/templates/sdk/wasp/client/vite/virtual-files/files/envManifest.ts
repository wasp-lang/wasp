// @ts-nocheck
{{={= =}=}}
{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
import { registerClientEnvSchema } from 'wasp/client/env/envRegistry'
registerClientEnvSchema({= envValidationSchema.importIdentifier =})
{=/ envValidationSchema.isDefined =}
