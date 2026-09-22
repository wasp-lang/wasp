import { type ProviderConfig, type RequestWithWasp } from '@wasp.sh/lib-sdk-core/node'
export { type ProviderConfig, type RequestWithWasp } from '@wasp.sh/lib-sdk-core/node'

import type { Prisma } from '@prisma/client'
import type { Exact, Expand, ProviderName } from '@wasp.sh/lib-sdk-core'
import type { FromRegister } from '@wasp.sh/lib-sdk-core'

// PUBLIC API
export function defineUserSignupFields<T extends UserSignupFields>(
  fields: Exact<UserSignupFields, T>
): T {
  return fields
}

// PUBLIC API
export type UserEmailSignupFields = InferUserSignupFields<RegisteredEmailSignupFields>;

type RegisteredEmailSignupFields = FromRegister<"emailUserSignupFields", {}>;

/**
 * Extracts the result types from a UserSignupFields object.
 * 
 * This type transforms an object containing field getter functions
 * into an object with the same keys but whose values are the return types
 * of those functions.
 */
type InferUserSignupFields<T extends UserSignupFields> = {
  [K in keyof T]: T[K] extends FieldGetter<PossibleUserFieldValues> 
    ? ReturnType<T[K]> 
    : never
}

type UserEntityCreateInput = Prisma.UserCreateInput

// PRIVATE API
export type PossibleUserFields = Expand<Partial<UserEntityCreateInput>>

// PRIVATE API
export type UserSignupFields = {
  [key in keyof PossibleUserFields]: FieldGetter<
    PossibleUserFields[key]
  >
}

type FieldGetter<T extends PossibleUserFieldValues> = (
  data: { [key: string]: unknown }
) => Promise<T | undefined> | T | undefined

type PossibleUserFieldValues = PossibleUserFields[keyof PossibleUserFields]
