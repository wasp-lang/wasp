{{={= =}=}}
{=# clientEnvValidationSchema.isDefined =}

declare module "{= clientEnvValidationSchema.importPath =}" {
  import type { RegisteredClientEnvValidationSchema } from "./client/env/schema";

  export const {= clientEnvValidationSchema.exportName =}: RegisteredClientEnvValidationSchema;
}
{=/ clientEnvValidationSchema.isDefined =}
{=# serverEnvValidationSchema.isDefined =}

declare module "{= serverEnvValidationSchema.importPath =}" {
  import type { RegisteredServerEnvValidationSchema } from "./server/env";

  export const {= serverEnvValidationSchema.exportName =}: RegisteredServerEnvValidationSchema;
}
{=/ serverEnvValidationSchema.isDefined =}
{=# prismaSetupFn.isDefined =}

declare module "{= prismaSetupFn.importPath =}" {
  import type { RegisteredPrismaSetupFn } from "./server/dbClient"

  export const {= prismaSetupFn.exportName =}: RegisteredPrismaSetupFn;
}
{=/ prismaSetupFn.isDefined =}
{=# actions =}

declare module "{= jsFn.importPath =}" {
  import { {= registeredOperationTypeName =} } from "./server/operations/actions/index";

  export const {= jsFn.exportName =}: {= registeredOperationTypeName =};
}
{=/ actions =}
{=# queries =}

declare module "{= jsFn.importPath =}" {
  import { {= registeredOperationTypeName =} } from "./server/operations/queries/index";

  export const {= jsFn.exportName =}: {= registeredOperationTypeName =};
}
{=/ queries =}
