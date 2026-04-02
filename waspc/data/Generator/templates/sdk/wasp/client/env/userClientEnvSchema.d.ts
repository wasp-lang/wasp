{{={= =}=}}
declare module '{= userClientEnvSchema.importPath =}' {
  import type { z } from 'zod'
  import type { FromRegistry } from 'wasp/types'

  type UserClientEnvSchema = FromRegistry<'clientEnvSchema', z.object<{}>>;
  export const {= userClientEnvSchema.exportName =}: UserClientEnvSchema;
}
