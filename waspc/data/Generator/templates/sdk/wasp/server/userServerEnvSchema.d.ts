{{={= =}=}}
{=# serverEnvSchema.isDefined =}
declare module '{= userServerEnvSchemaPath =}' {
  import type { z } from 'zod'
  import type { FromRegistry } from 'wasp/types'

  export const {= serverEnvSchema.importIdentifier =}: FromRegistry<'serverEnvSchema', z.ZodObject<{}>>
}
{=/ serverEnvSchema.isDefined =}
