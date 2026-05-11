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
 * Types `XFromRegister` safely read values from these registers:
 *  - If the user provided a type → it is used
 *  - Otherwise → a fallback type is used
 */

export interface Register {}

export type FromRegister<Key extends string, Fallback> = Key extends keyof Register
  ? Register[Key]
  : Fallback;

export type OperationFromRegister<
  Operation extends string,
  Fallback,
  Subregister = "operations",
> = Subregister extends keyof Register
  ? Operation extends keyof Register[Subregister]
    ? Register[Subregister][Operation]
    : Fallback
  : Fallback;

export type CrudOverrideFromRegister<
  CrudName extends string,
  CrudOperation extends string,
  Fallback,
  Subregister = "crudOverrides",
>  = Subregister extends keyof Register
  ? CrudName extends keyof Register[Subregister]
    ? CrudOperation extends keyof Register[Subregister][CrudName]
      ? Register[Subregister][CrudName][CrudOperation]
      : Fallback
    : Fallback
  : Fallback;
