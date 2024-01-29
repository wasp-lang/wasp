import type { Router, Request } from 'express'
import type { Prisma } from '@prisma/client'
import type { Expand } from 'wasp/universal/types'
import type { ProviderName } from '../utils'

// PUBLIC API
export function defineUserSignupFields(fields: UserSignupFields) {
  return fields
}

type UserEntityCreateInput = Prisma.UserCreateInput

// PRIVATE API
export type ProviderConfig = {
    // Unique provider identifier, used as part of URL paths
    id: ProviderName;
    displayName: string;
    // Each provider config can have an init method which is ran on setup time
    // e.g. for oAuth providers this is the time when the Passport strategy is registered.
    init?(provider: ProviderConfig): Promise<InitData>;
    // Every provider must have a setupRouter method which returns the Express router.
    // In this function we are flexibile to do what ever is necessary to make the provider work.
    createRouter(provider: ProviderConfig, initData: InitData): Router;
};

// PRIVATE API
export type InitData = {
    [key: string]: any;
}

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

type FieldGetter<T> = (
  data: { [key: string]: unknown }
) => Promise<T | undefined> | T | undefined
