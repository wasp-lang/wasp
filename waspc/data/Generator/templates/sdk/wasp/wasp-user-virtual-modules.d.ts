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
  import type { FromOperationsRegistry } from "wasp/types";
  import type { {= operationTypeName =} } from "wasp/server/operations";

  export type {= operationResolvedTypeName =} = FromOperationsRegistry<'{= operationName =}', {= operationTypeName =}>;
  export const {= jsFn.exportName =}: {= operationResolvedTypeName =};
}
{=/ actions =}
{=# queries =}

declare module "{= jsFn.importPath =}" {
  import type { FromOperationsRegistry } from "wasp/types";
  import type { {= operationTypeName =} } from "wasp/server/operations";

  export type {= operationResolvedTypeName =} = FromOperationsRegistry<'{= operationName =}', {= operationTypeName =}>;
  export const {= jsFn.exportName =}: {= operationResolvedTypeName =};
}
{=/ queries =}
