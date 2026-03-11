{{={= =}=}}
declare module '{= userClientEnvSchema.importPath =}' {
  import type { z } from 'zod'
  import type { FromRegistry } from 'wasp/types'

  export const {= userClientEnvSchema.exportName =}: FromRegistry<'clientEnvSchema', z.ZodObject<{}>>
}
