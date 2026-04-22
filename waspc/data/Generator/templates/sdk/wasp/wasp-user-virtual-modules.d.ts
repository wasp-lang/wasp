{{={= =}=}}
declare module "{= clientEnvValidationSchema.importPath =}" {
  import type { FromRegistry } from "wasp/types";
  import type z from "zod"

  export type UserClientEnvSchema = FromRegistry<"clientEnvSchema", z.ZodObject<{}>>;
  export const {= clientEnvValidationSchema.exportName =}: UserClientEnvSchema;
}

declare module "{= serverEnvValidationSchema.importPath =}" {
  import type { FromRegistry } from "wasp/types";
  import type z from "zod"

  export type UserServerEnvSchema = FromRegistry<"serverEnvSchema", z.ZodObject<{}>>;
  export const {= serverEnvValidationSchema.exportName =}: UserServerEnvSchema;
}

declare module "{= prismaSetupFn.importPath =}" {
  import type { FromRegistry } from "wasp/types"
  import type { PrismaClient as InternalPrismaClient } from "@prisma/client"

  export type UserPrismaSetupFn = FromRegistry<"prismaSetupFn", () => InternalPrismaClient>;
  export type PrismaClientResolved = ReturnType<UserPrismaSetupFn>;
  export const {= prismaSetupFn.exportName =}: UserPrismaSetupFn;
}
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
