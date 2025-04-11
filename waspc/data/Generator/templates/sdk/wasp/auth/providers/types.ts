{{={= =}=}}
import type { Router, Request } from 'express'
import type { Prisma } from '@prisma/client'
import type { Expand } from 'wasp/universal/types'
import type { ProviderName } from '../utils'


{=# emailUserSignupFields.isDefined =}
{=& emailUserSignupFields.importStatement =}
const _waspEmailUserSignupFields = {= emailUserSignupFields.importIdentifier =}
{=/ emailUserSignupFields.isDefined =}
{=^ emailUserSignupFields.isDefined =}
const _waspEmailUserSignupFields = undefined
{=/ emailUserSignupFields.isDefined =}

{=# usernameAndPasswordUserSignupFields.isDefined =}
{=& usernameAndPasswordUserSignupFields.importStatement =}
const _waspUsernameAndPasswordUserSignupFields = {= usernameAndPasswordUserSignupFields.importIdentifier =}
{=/ usernameAndPasswordUserSignupFields.isDefined =}
{=^ usernameAndPasswordUserSignupFields.isDefined =}
const _waspUsernameAndPasswordUserSignupFields = undefined
{=/ usernameAndPasswordUserSignupFields.isDefined =}


// PUBLIC API
export function defineUserSignupFields<T extends UserSignupFields>(
  fields: T
): T {
  return fields
}

// PUBLIC API
export type UserEmailSignupFields = InferUserSignupFields<typeof _waspEmailUserSignupFields>;

// PUBLIC API
export type UserUsernameAndPasswordSignupFields = InferUserSignupFields<typeof _waspUsernameAndPasswordUserSignupFields>;

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
export type PossibleUserFields = Expand<Partial<Omit<UserEntityCreateInput, 'auth'>>>

// PRIVATE API
export type UserSignupFields = {
  [key in keyof PossibleUserFields]: FieldGetter<
    PossibleUserFields[key]
  >
}

type FieldGetter<T> = (
  data: { [key: string]: unknown }
) => Promise<T | undefined> | T | undefined

export type InferUserSignupFields<T> = {
  [K in keyof T]: T[K] extends (data: any) => infer R ? R : never
}
