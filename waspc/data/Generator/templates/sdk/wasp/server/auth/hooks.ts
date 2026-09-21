import type { Request as ExpressRequest } from 'express'
import type { ProviderId, FindAuthWithUserResult } from './utils.js'
import type { CreateUserResult } from './identityStore.js'
import type { OAuthTokens } from './handler/types.js'
import { prisma } from '../index.js'
import type { Expand } from '../../universal/types.js'

// PUBLIC API
export type OnBeforeSignupHook = (
  params: Expand<OnBeforeSignupHookParams>,
) => void | Promise<void>

// PUBLIC API
export type OnAfterSignupHook = (
  params: Expand<OnAfterSignupHookParams>,
) => void | Promise<void>

// PUBLIC API
export type OnBeforeLoginHook = (
  params: Expand<OnBeforeLoginHookParams>,
) => void | Promise<void>

// PUBLIC API
export type OnAfterLoginHook = (
  params: Expand<OnAfterLoginHookParams>,
) => void | Promise<void>

// PUBLIC API
/** Called before a new identity is attached to an existing account. Throw to veto. */
export type OnBeforeLinkHook = (
  params: Expand<OnBeforeLinkHookParams>,
) => void | Promise<void>

// PUBLIC API
/** Called after a new identity was attached to an existing account. */
export type OnAfterLinkHook = (
  params: Expand<OnAfterLinkHookParams>,
) => void | Promise<void>

// PUBLIC API
/**
 * Use this type for your `auth.mergeUsers` function. It runs inside the merge
 * transaction, BEFORE Wasp moves the identities and deletes `from`: re-point
 * everything `from` owns to `into`, and decide whose profile fields win.
 * Use the given `prisma` (the transaction client) so your writes roll back
 * with the merge. Throw to abort it.
 */
export type MergeUsersFn = (params: {
  /** The user that is going away. */
  from: FindAuthWithUserResult['user']
  /** The surviving user: the one who is signed in. */
  into: FindAuthWithUserResult['user']
  /** The merge's transaction client. */
  prisma: Parameters<Parameters<typeof prisma.$transaction>[0]>[0]
  req?: ExpressRequest
}) => void | Promise<void>

// PRIVATE API (used in the SDK and the server)
export type InternalAuthHookParams = {
  /**
   * Prisma instance that can be used to interact with the database.
  */
  prisma: typeof prisma
}

// NOTE: We should be exporting types that can be reached by users via other
// exported types (e.g. using the Parameters<T> Typescript helper).
// However, we are not exporting this type to keep the API surface smaller.
// This type is only used internally by the SDK. Exporting it might confuse
// users since the name is too similar to the exported function type.
// Same goes for all other *Params types in this file.
type OnBeforeSignupHookParams = {
  /**
   * Provider ID object that contains the provider name and the provider user ID.
  */
  providerId: ProviderId
  /**
   * Request object that can be used to access the user's incoming signup request.
  */
  req?: ExpressRequest
} & InternalAuthHookParams

type OnAfterSignupHookParams = {
  /**
   * Provider ID object that contains the provider name and the provider user ID.
  */
  providerId: ProviderId
  /**
   * User object that was created during the signup process.
  */
  user: CreateUserResult
  /**
   * OAuth flow data that was generated during the OAuth flow. This is only
   * available if the user signed up using OAuth.
  */
  oauth?: OAuthData
  /**
   * Request object that can be used to access the user's incoming signup request.
  */
  req?: ExpressRequest
} & InternalAuthHookParams

type OnBeforeLoginHookParams = {
  /**
   * Provider ID object that contains the provider name and the provider user ID.
  */
  providerId: ProviderId
  /**
   * User that is trying to log in.
  */
  user: FindAuthWithUserResult['user']
  /**
   * Request object that can be used to access the user's incoming login request.
  */
  req?: ExpressRequest
} & InternalAuthHookParams

type OnBeforeLinkHookParams = {
  /**
   * The identity being attached: its namespace and the provider's user ID.
  */
  providerId: ProviderId
  /**
   * The signed-in user the identity is being attached to.
  */
  user: FindAuthWithUserResult['user']
  /**
   * Request object of the incoming link request.
  */
  req?: ExpressRequest
} & InternalAuthHookParams

type OnAfterLinkHookParams = OnBeforeLinkHookParams & {
  /**
   * OAuth flow data, when the identity was linked through an OAuth flow.
  */
  oauth?: OAuthData
}

type OnAfterLoginHookParams = {
  /**
   * Provider ID object that contains the provider name and the provider user ID.
  */
  providerId: ProviderId
  /**
   * User that is logged in.
  */
  user: FindAuthWithUserResult['user']
  /**
   * OAuth flow data that was generated during the OAuth flow. This is only
   * available if the user logged in using OAuth.
  */
  oauth?: OAuthData
  /**
   * Request object that can be used to access the user's incoming login request.
  */
  req?: ExpressRequest
} & InternalAuthHookParams

// PUBLIC API
/**
 * What a signup, login or link through an OAuth provider carries: the unique
 * request id the app saw in `onBeforeOAuthRedirect`, the provider's name and
 * the provider's tokens. The auth handler hands it to Wasp, which requires
 * it for every provider declared with `kind: "oauth"`.
 */
export type OAuthData = {
  /**
   * Unique request ID that was generated during the OAuth flow.
  */
  uniqueRequestId: string
  providerName: string
  tokens: OAuthTokens
}
