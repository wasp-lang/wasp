{{={= =}=}}
{=# clientEnvValidationSchema.isDefined =}

declare module "{= clientEnvValidationSchema.importPath =}" {
  import type { UserServerEnvSchema } from "./client/env/schema";

  export const {= clientEnvValidationSchema.exportName =}: UserClientEnvSchema;
}
{=/ clientEnvValidationSchema.isDefined =}
{=# serverEnvValidationSchema.isDefined =}

declare module "{= serverEnvValidationSchema.importPath =}" {
  import type { UserServerEnvSchema } from "./server/env";

  export const {= serverEnvValidationSchema.exportName =}: UserServerEnvSchema;
}
{=/ serverEnvValidationSchema.isDefined =}
{=# prismaSetupFn.isDefined =}

declare module "{= prismaSetupFn.importPath =}" {
  import type { PrismaClientResolved } from "./server/dbClient"

  export const {= prismaSetupFn.exportName =}: UserPrismaSetupFn;
}
{=/ prismaSetupFn.isDefined =}
{=# actions =}

declare module "{= jsFn.importPath =}" {
  import { {= operationResolvedTypeName =} } from "./server/operations/actions/index";

  export const {= jsFn.exportName =}: {= operationResolvedTypeName =};
}
{=/ actions =}
{=# queries =}

declare module "{= jsFn.importPath =}" {
  import { {= operationResolvedTypeName =} } from "./server/operations/queries/index";

  export const {= jsFn.exportName =}: {= operationResolvedTypeName =};
}
{=/ queries =}
