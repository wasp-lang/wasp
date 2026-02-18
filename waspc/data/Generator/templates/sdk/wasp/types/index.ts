// PRIVATE API
/**
 * These interfaces are augmented in .wasp/out/types/ to register
 * user-defined types into the SDK's type system.
 */
export interface RegisteredOperations {}
export interface RegisteredCrudOverrides {}
export interface RegisteredConfig {}

// PRIVATE API
/**
 * Looks up an operation key in RegisteredOperations, returning the registered
 * type if present or the provided Default otherwise.
 */
export type GetOperationFromRegistry<K extends string, Default> =
  K extends keyof RegisteredOperations
    ? RegisteredOperations[K]
    : Default

// PRIVATE API
/**
 * Looks up a CRUD override by CRUD name and operation name.
 */
export type GetCrudOverrideFromRegistry<CrudName extends string, Op extends string, Default> =
  CrudName extends keyof RegisteredCrudOverrides
    ? Op extends keyof RegisteredCrudOverrides[CrudName]
      ? RegisteredCrudOverrides[CrudName][Op]
      : Default
    : Default

// PRIVATE API
/**
 * Looks up a config key in RegisteredConfig, returning the registered type
 * if present or the provided Default otherwise.
 */
export type GetConfigFromRegistry<K extends string, Default> =
  K extends keyof RegisteredConfig
    ? RegisteredConfig[K]
    : Default
