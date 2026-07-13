/**
 * This module acts as a bridge between SDK and user project types.
 * 
 * If the SDK tried to import types from the user project directly,
 * it would create a cyclic dependency between TypeScript projects.
 * TypeScript can't compile projects that have cyclic dependencies.
 * So we must find a way to bridge the types without the SDK depending
 * on the user project.
 * 
 * Copying the user project and making the SDK depend on the copy
 * is not an option, becuase it forces the user project to compile
 * with the SDK's TypeScript config.
 * ({@link https://github.com/wasp-lang/wasp/issues/2247 Old issue about the problem})
 *
 * Instead, the solution is for the user project to push types into
 * the SDK. That way the SDK can use the user's types, without 
 * depending on the user project.
 * 
 * The SDK defines {@link Register} interface, which is publicly
 * exported through the `wasp/types` module.
 * 
 * During compilation, Wasp generates additional type declarations in
 * `.wasp/out/types/app/sdk/register.ts` (which is part of the user project)
 * that extend empty {@link Register} interface via module augmentation
 * and declaration merging. This essentially pushes user project types
 * into the SDK.
 * 
 * On the SDK side, all user project dependent types are resolved through
 * the {@link Register} interface. If a user-defined type for something
 * exists in {@link Register}, we use it; otherwise, we fallback to some
 * sensible default type. We must have a fallback because the SDK must 
 * be compileable standalone, without a user project.
 * 
 * As a result, users can see their own user-defined types in the SDK,
 * without SDK, directly depending on the user code.
 *
 * @see {@link https://www.typescriptlang.org/docs/handbook/declaration-merging.html#merging-interfaces Interface declaration merging in TypeScript}
 * @see {@link https://www.typescriptlang.org/docs/handbook/declaration-merging.html#module-augmentation Module augmentation in TypeScript}
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
