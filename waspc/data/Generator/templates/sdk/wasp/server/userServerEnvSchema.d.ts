{{={= =}=}}
declare module '{= userServerEnvSchema.importPath =}' {
  import type { z } from 'zod'
  import type { FromRegistry } from 'wasp/types'

  type UserServerEnvSchema = FromRegistry<'serverEnvSchema', z.object<{}>>;
  export const {= userServerEnvSchema.exportName =}: UserServerEnvSchema;
}
