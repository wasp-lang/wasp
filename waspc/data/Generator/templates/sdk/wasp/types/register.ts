/**
 * This module acts as a bridge between SDK and user project types.
 * 
 * If SDK tried to directly import types from user project,
 * it would create a cyclic dependency between TypeScript projects.
 * Instead, the solution is for user project to push types into SDK.
 * That way the SDK can use user's types, without knowning about
 * the user project.
 *
 * The SDK defines {@link Register} interface, which is publicly
 * exported through the `wasp/types` module.
 * 
 * During compilation, Wasp generate additional type declarations in
 * `.wasp/out/types/app/sdk` (which is part of user project) that
 * extend empty {@link Register} interface via module augmentation:
 *  ```ts
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
 * the {@link Register} interface. If a user defined type for something
 * exists in {@link Register}, we use it, otherwise, we default to
 * something sesnible.
 * E.g. `PrismaClient` type is either type of user's custom intance,
 * or a type of default prisma client instance.
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
