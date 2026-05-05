/**
 * This module acts as a bridge between the SDK and user-defined types.
 *
 * The SDK defines and exports empty "register" interfaces (e.g. {@link Register}). 
 * During compliation, Wasp generate additional type declarations
 * in `.wasp/out/types/` (which is part of user project) that extend 
 * empty "register" interfaces via module augmentation.
 *
 * As a result, the SDK can "see" user-defined types without directly
 * depending on user code. 
 * 
 * Types `FromXRegister` safely read values from these registers:
 *  - If the user provided a type → it is used
 *  - Otherwise → a fallback type is used
 */

/**
 * For registring types from the user project.
 * Types without a more specific register go here.
 */
export interface Register {}
export type FromRegister<Key extends string, Fallback> = Key extends keyof Register
  ? Register[Key]
  : Fallback;

/**
 * For registring operation types from the user project.
 */
export interface OperationsRegister {}
export type FromOperationsRegister<
  Operation extends string,
  Fallback,
> = Operation extends keyof OperationsRegister
  ? OperationsRegister[Operation]
  : Fallback;

  /**
 * For registring CRUD overrides types from the user project.
 */
export interface CrudOverridesRegister {}
export type FromCrudOverridesRegister<
  CrudName extends string,
  CrudOperation extends string,
  Fallback,
> = CrudName extends keyof CrudOverridesRegister
  ? CrudOperation extends keyof CrudOverridesRegister[CrudName]
    ? CrudOverridesRegister[CrudName][CrudOperation]
    : Fallback
  : Fallback;
