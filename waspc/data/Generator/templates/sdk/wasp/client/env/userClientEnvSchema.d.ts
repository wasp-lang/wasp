{{={= =}=}}
declare module '{= userClientEnvSchemaPath =}' {
  import type { z } from 'zod'
  import type { FromRegistry } from 'wasp/types'

  export const userClientEnvSchema: FromRegistry<'clientEnvSchema', z.ZodObject<{}>>
}
