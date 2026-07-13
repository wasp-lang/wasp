/**
 * This module acts as a bridge between SDK and user project types.
 *
 * The SDK defines and exports empty {@link Register} interface.
 * 
 * During compilation, Wasp generate additional type declarations
 * in `.wasp/out/types/runtime/sdk` (which is part of user project) that 
 * extend (augment) empty {@link Register} interface via module augmentation:
 * ```ts
 * declare module "wasp/types" {
 *   interface Register {
 *     prismaSetupFn: typeof import('../../../../../src/features/db/prisma').setUpPrisma
 *     // ...
 *   }
 * }
 * ```
 * This essentially pushes user project types into SDK.
 * 
 * On the SDK side, all user project dependent types are resolved through
 * the {@link Register} interface:
 * ```ts
 * type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', () => InternalPrismaClient>;
 * export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;
 * ```
 * This ensures user project types are reflected in the SDK.
 * E.g., operation clients will accept parameters and have return types that the user defined.
 * 
 * As a result, users can see their own user-defined types in SDK,
 * without SDK directly depending on the user code.
 */

/**
 * Register for type augmentation via declaration merging.
 */
export interface Register {}

/**
 * Safely reads values from {@link Register}.
 * Returns the registered type for a given {@link Key}, or falls back to {@link Fallback}.
 */
export type FromRegister<Key extends string, Fallback> = Key extends keyof Register
  ? Register[Key]
  : Fallback;
