{{={= =}=}}
declare module '{= userClientEnvSchemaPath =}' {
  import type { z } from 'zod'
  import type { GetConfigFromRegistry } from 'wasp/types'

  export const userClientEnvSchema: GetConfigFromRegistry<'clientEnvSchema', z.ZodObject<{}>>
}
