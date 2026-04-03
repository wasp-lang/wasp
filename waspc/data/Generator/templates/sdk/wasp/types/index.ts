/**
 * The interfaces in this module are augmented by types from `.wasp/out/types/`.
 * They are agumented with user declarations types.
 *
 * E.g., a user defined prisma client.
 */

export interface Registry {}
export type FromRegistry<K extends string, Default> = K extends keyof Registry
  ? Registry[K]
  : Default;

export interface OperationsRegistry {}
export type FromOperationsRegistry<
  Operation extends string,
  Default,
> = Operation extends keyof OperationsRegistry
  ? OperationsRegistry[Operation]
  : Default;

export interface CrudOverridesRegistry {}
export type FromCrudOverridesRegistry<
  CrudName extends string,
  CrudOperation extends string,
  Default,
> = CrudName extends keyof CrudOverridesRegistry
  ? CrudOperation extends keyof CrudOverridesRegistry[CrudName]
    ? CrudOverridesRegistry[CrudName][CrudOperation]
    : Default
  : Default;
