{{={= =}=}}
declare module "{= clientEnvValidationSchemaVMId =}" {
  import type { FromRegistry } from "wasp/types";
  import type z from "zod"

  export type UserClientEnvSchema = FromRegistry<"clientEnvSchema", z.ZodObject<{}>>;
  {=# clientEnvValidationSchema.isDefined =}
  export const {= clientEnvValidationSchema.exportName =}: UserClientEnvSchema;
  {=/ clientEnvValidationSchema.isDefined =}
}

declare module "{= serverEnvValidationSchemaVMId =}" {
  import type { FromRegistry } from "wasp/types";
  import type z from "zod"

  export type UserServerEnvSchema = FromRegistry<"serverEnvSchema", z.ZodObject<{}>>;
  {=# serverEnvValidationSchema.isDefined =}
  export const {= serverEnvValidationSchema.exportName =}: UserServerEnvSchema;
  {=/ serverEnvValidationSchema.isDefined =}
}

declare module "{= prismaSetupFnVMId =}" {
  import type { FromRegistry } from "wasp/types"
  import type { PrismaClient as InternalPrismaClient } from "@prisma/client"

  export type UserPrismaSetupFn = FromRegistry<"prismaSetupFn", () => InternalPrismaClient>;
  export type PrismaClientResolved = ReturnType<UserPrismaSetupFn>;
  {=# prismaSetupFn.isDefined =}
  export const {= prismaSetupFn.exportName =}: UserPrismaSetupFn;
  {=/ prismaSetupFn.isDefined =}
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
