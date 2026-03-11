{{={= =}=}}
declare module '{= userServerEnvSchema.importPath =}' {
  import type { z } from 'zod'
  import type { FromRegistry } from 'wasp/types'

  export const {= userServerEnvSchema.exportName =}: FromRegistry<'serverEnvSchema', z.ZodObject<{}>>
}
