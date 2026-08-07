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
