/**
 * This module allows the SDK to use the user project types.
 * The other part, values, is document in
 * `waspc/src/Wasp/Generator/SdkGenerator/JsImport.hs`.
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
 * Instead, the SDK defines an extension point (empty {@link Register}
 * interface) that the user project extends. The SDK never imports from
 * the user project, it only references the extension point, which
 * resolves to the user's types when TypeScript compiles the user project.
 *
 * {@link Register} is publicly exported through the `wasp/types` module.
 * During compilation, Wasp generates type declarations in 
 * `.wasp/out/types/app/sdk/register.ts` (part of the user project) that
 * extend {@link Register} via module augmentation and declaration merging.
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
 *    everything the SDK expects of them, the SDK can compile on its own.
 *
 *    E.g., during SDK compilation, the `PrismaClient` type resolves to
 *    its fallback: a Prisma client with default settings. This satisfies
 *    all SDK's expectations of the `PrismaClient` type.
 * 
 * 2. The conditional types allow for the propagation of types from the SDK
 *    back to the user project. Since we force the types to stay in their
 *    conditional (rather than resolved) form, as soon as the condition
 *    changes the type itself is recalculated. That means as soon as users
 *    updates their types, the SDK's types will also recalculate.
 * 
 * 2. They let the same SDK declarations resolve to different types in
 *    different projects. We force SDK's emitted declarations to stay
 *    in their conditional (rather than resolved) form, so when
 *    TypeScript checks the user project, where {@link Register} is
 *    extended, those same conditional types resolve to the user's
 *    types instead of the fallbacks.
 * 
 *    E.g., if a user defines a custom Prisma client instance, the
 *    `PrismaClient` type will instead return the user's custom client.
 *
 * Above we said we "force the types to stay in their conditional (rather
 * than resolved) form". Why do we have to force them?
 *
 * TypeScript resolves inferred types when emitting declaration files.
 * Since the SDK compiles with an empty {@link Register}, an inferred type
 * ends up in the emitted `.d.ts` file as the already-resolved fallback:
 * ```ts
 * // Inferred: emitted already resolved to the fallback.
 * declare const dbClient: InternalPrismaClient;
 * ```
 * Therefore, everything in the SDK derived from a registered type must be
 * explicitly typed (`: RegisteredType`, `as RegisteredType`, or
 * `<T extends RegisteredType>`) to force TypeScript to keep its conditional
 * form in the emitted declarations:
 * ```ts
 * // Explicitly typed: emitted in its conditional form.
 * declare const dbClient: ReturnType<FromRegister<'prismaSetupFn', () => InternalPrismaClient>>;
 * ```
 * 
 * @see {@link https://github.com/wasp-lang/wasp/pull/4049 PR implementing the change}
 */

/**
 * Register for type augmentation via declaration merging.
 */
export interface Register {}

/**
 * Safely reads a value from {@link Register} by following {@link Path}.
 * Returns the registered type, or {@link Fallback} if any step of the path is missing.
 * The path can be of any depth.
 */
export type FromRegisterPath<Path extends readonly string[], Fallback> =
  WalkRegisterPath<Register, Path, Fallback>;

type WalkRegisterPath<Current, Path extends readonly string[], Fallback> =
  Path extends readonly [infer Key extends string, ...infer Rest extends readonly string[]]
    ? Key extends keyof Current
      ? WalkRegisterPath<Current[Key], Rest, Fallback>
      : Fallback
    : Current;

/**
 * Shorthand for reading a top-level {@link Register} value.
 */
export type FromRegister<Key extends string, Fallback> = FromRegisterPath<[Key], Fallback>;
