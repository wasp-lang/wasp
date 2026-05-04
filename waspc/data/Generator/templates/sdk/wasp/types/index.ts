/**
 * The interfaces in this module are augmented by types from `.wasp/out/types/`.
 * They are agumented with user declarations types.
 *
 * E.g., a user defined prisma client.
 */

export interface Register {}
export type FromRegister<K extends string, Default> = K extends keyof Register
  ? Register[K]
  : Default;

export interface OperationsRegister {}
export type FromOperationsRegister<
  Operation extends string,
  Default,
> = Operation extends keyof OperationsRegister
  ? OperationsRegister[Operation]
  : Default;

export interface CrudOverridesRegister {}
export type FromCrudOverridesRegister<
  CrudName extends string,
  CrudOperation extends string,
  Default,
> = CrudName extends keyof CrudOverridesRegister
  ? CrudOperation extends keyof CrudOverridesRegister[CrudName]
    ? CrudOverridesRegister[CrudName][CrudOperation]
    : Default
  : Default;
