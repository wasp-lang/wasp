/**
 * This module allows the SDK to use the user project types.
 * 
 * If the SDK tried to import types from the user project directly,
 * it would create a cyclic dependency between TypeScript projects.
 * TypeScript can't compile projects that have cyclic dependencies.
 * So we must find a way to use the user project types without the
 * SDK depending on the user project.
 * 
 * Copying the user project and making the SDK depend on the copy
 * is not an option, because it forces the user's project to compile
 * with the SDK's TypeScript config.
 * ({@link https://github.com/wasp-lang/wasp/issues/2247 Old issue about the problem})
 *
 * Instead, the solution is for the user project to push types into
 * the SDK. That way the SDK can use the user's types, without
 * depending on the user project.
 * 
 * The SDK defines empty {@link Register} interface, which is publicly
 * exported through the `wasp/types` module.
 * 
 * During compilation, Wasp generates additional type declarations in
 * `.wasp/out/types/app/sdk/register.ts` (which is part of the user project)
 * that extend empty {@link Register} interface via module augmentation
 * and declaration merging. This essentially pushes user project types
 * into the SDK.
 * 
 * On the SDK side, all user project dependent types are defined as
 * conditional types. If a user-defined type for something exists in
 * {@link Register}, we use it; otherwise, we fallback to some
 * sensible default type.
 * 
 * The purpose of conditional types in SDK is two-fold:
 * 
 * 1. They allow the SDK to compile on its own (without a user project).
 *    The SDK compiles before any other TypeScript project, so at that
 *    point {@link Register} is always empty and every conditional type
 *    resolves to its fallback. By having the fallback types satisfy
 *    everything the SDK expects of them, the SDK compiles on its own.
 *
 *    E.g., during SDK compilation, the `PrismaClient` type resolves to
 *    its fallback: a Prisma client with default settings. This satisfies
 *    all SDK's expectations of `PrismaClient` type.
 * 
 * 2. The conditional types allow for the propagation of types from the SDK
 *    back to the user project. Since we force the types to stay in their
 *    conditional (rather than resolved) form, as soon as the condition
 *    changes the type itself is recalculated. That means as soon as users
 *    updates their types, the SDK types will also recalculate.
 * 
 *    E.g., if a user defines a custom Prisma client instance, the
 *    `PrismaClient` type will instead return the user's custom client.
 *
 * Above we said we "force the types to stay in their conditional (rather
 * than resolved) form". Why force?
 *
 * TypeScript resolves inferred types when emitting declaration files.
 * Since the SDK compiles with an empty {@link Register}, an inferred type
 * ends up in the emitted `.d.ts` file as the already-resolved fallback
 * and never recalculates for the user:
 * ```ts
 * // Inferred: emitted already resolved to the fallback.
 * declare const dbClient: InternalPrismaClient;
 * // Explicitly typed: emitted in its conditional form.
 * declare const dbClient: ReturnType<FromRegister<'prismaSetupFn', () => InternalPrismaClient>>;
 * ```
 * Therefore, everything in the SDK derived from a registered type must be
 * explicitly typed (`: RegisteredType`, `as RegisteredType`, or
 * `<T extends RegisteredType>`) to keep it in its conditional form in the
 * emitted declarations.
 *
 * As a result, users can see their own user-defined types in the SDK,
 * without SDK, directly depending on the user code.
 *
 * @see {@link https://github.com/wasp-lang/wasp/pull/4049 PR implementing the change}
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
