{{={= =}=}}
import type { Router, Request } from 'express'
import type { Prisma } from '@prisma/client'
import type { Expand, Exact } from '../../universal/types'
import type { ProviderName } from '../utils'

// PUBLIC API
export function defineUserSignupFields<T extends UserSignupFields>(
  fields: Exact<UserSignupFields, T>
): T {
  return fields
}

{=# isEmailUserSignupFieldsDefined =}
import type { FromRegister } from '../../types/register'

// PUBLIC API
export type UserEmailSignupFields = InferUserSignupFields<RegisteredEmailSignupFields>;

type RegisteredEmailSignupFields = FromRegister<"emailUserSignupFields", {}>;
{=/ isEmailUserSignupFieldsDefined =}

{=# isUsernameAndPasswordUserSignupFieldsDefined =}
import type { FromRegister } from '../../types/register'

// PUBLIC API
export type UserUsernameAndPasswordSignupFields = InferUserSignupFields<RegisteredUsernameAndPasswordSignupFields>;

type RegisteredUsernameAndPasswordSignupFields = FromRegister<"usernameAndPasswordUserSignupFields", {}>
{=/ isUsernameAndPasswordUserSignupFieldsDefined =}

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

type UserEntityCreateInput = Prisma.{= userEntityUpper =}CreateInput

// PRIVATE API
export type ProviderConfig = {
    // Unique provider identifier, used as part of URL paths
    id: ProviderName;
    displayName: string;
    // Every provider must have a setupRouter method which returns the Express router.
    // In this function we are flexibile to do what ever is necessary to make the provider work.
    createRouter(provider: ProviderConfig): Router;
};

// PRIVATE API
export type RequestWithWasp = Request & { wasp?: { [key: string]: any } }

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
