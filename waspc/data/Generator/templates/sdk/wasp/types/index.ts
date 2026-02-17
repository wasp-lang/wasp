// PRIVATE API
/**
 * This interface is augmented in .wasp/out/types/ to register
 * user-defined types (e.g., signup fields) into the SDK's type system.
 */
export interface Register {}

// PRIVATE API
/**
 * Looks up a key in the Register interface, returning the registered type
 * if present or the provided Default otherwise. This must be exported so
 * TypeScript preserves it by name in .d.ts output — if inlined, the
 * conditional is eagerly resolved before module augmentation can take effect.
 */
export type GetFromRegister<K extends string, Default> =
  K extends keyof Register
    ? Register[K]
    : Default
