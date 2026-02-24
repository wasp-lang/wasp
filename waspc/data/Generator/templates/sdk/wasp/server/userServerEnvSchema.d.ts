{{={= =}=}}
declare module '{= userServerEnvSchemaPath =}' {
  import type { z } from 'zod'
  import type { FromRegistry } from 'wasp/types'

  export const userServerEnvSchema: FromRegistry<'serverEnvSchema', z.ZodObject<{}>>
}
