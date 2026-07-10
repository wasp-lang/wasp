/**
 * This module acts as a bridge between the SDK and user-defined types.
 *
 * The SDK defines and exports empty {@link Register} interface.
 * During compliation, Wasp generate additional type declarations
 * in `.wasp/out/types/sdk/` (which is part of user project) that extend
 * the empty {@link Register} interface via module augmentation.
 *
 * As a result, users can "see" user-defined types in SDK without SDK 
 * directly depending on the user code.
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
