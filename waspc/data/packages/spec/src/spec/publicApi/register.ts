/**
 * Registry for type augmentation via declaration merging.
 *
 * @internal
 */
export interface Register {}

export type FromRegister<
  Key extends string,
  Fallback,
> = Key extends keyof Register ? Register[Key] : Fallback;
