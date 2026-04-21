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
