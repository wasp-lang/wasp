import type { Request as ExpressRequest } from 'express'
import { type ProviderId, createUser, findAuthWithUserBy } from '../../auth/utils.js'
import { prisma } from '../index.js'
import { Expand } from '../../universal/types.js'
import { ProviderName } from '../_types/index.js';

// PUBLIC API
export type OnBeforeSignupHook = (
  params: Expand<OnBeforeSignupHookParams>,
) => void | Promise<void>

// PUBLIC API
export type OnAfterSignupHook = (
  params: Expand<OnAfterSignupHookParams>,
) => void | Promise<void>

// PUBLIC API
/**
 * @returns Object with a URL that the OAuth flow should redirect to.
 */
export type OnBeforeOAuthRedirectHook = (
  params: Expand<OnBeforeOAuthRedirectHookParams>,
) => { url: URL } | Promise<{ url: URL }>

// PUBLIC API
export type OnBeforeLoginHook = (
  params: Expand<OnBeforeLoginHookParams>,
) => void | Promise<void>

// PUBLIC API
export type OnAfterLoginHook = (
  params: Expand<OnAfterLoginHookParams>,
) => void | Promise<void>

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
   * Provider ID object that contains the provider name and the provide user ID.
  */
  providerId: ProviderId
  /**
   * Request object that can be used to access the incoming request.
  */
  req: ExpressRequest
} & InternalAuthHookParams

type OnAfterSignupHookParams = {
  /**
   * Provider ID object that contains the provider name and the provide user ID.
  */
  providerId: ProviderId
  /**
   * User object that was created during the signup process.
  */
  user: Awaited<ReturnType<typeof createUser>>
  /**
   * OAuth flow data that was generated during the OAuth flow. This is only
   * available if the user signed up using OAuth.
  */
  oauth?: OAuthParams
  /**
   * Request object that can be used to access the incoming request.
  */
  req: ExpressRequest
} & InternalAuthHookParams

type OnBeforeOAuthRedirectHookParams = {
  /**
   * URL that the OAuth flow should redirect to.
  */
  url: URL
  /**
   * Unique request ID that was generated during the OAuth flow.
  */
  uniqueRequestId: OAuthParams['uniqueRequestId']
  /**
   * Request object that can be used to access the incoming request.
  */
  req: ExpressRequest
} & InternalAuthHookParams

type OnBeforeLoginHookParams = {
  /**
   * Provider ID object that contains the provider name and the provide user ID.
  */
  providerId: ProviderId
  /**
   * Request object that can be used to access the incoming request.
  */
  req: ExpressRequest
} & InternalAuthHookParams

type OnAfterLoginHookParams = {
  /**
   * Provider ID object that contains the provider name and the provide user ID.
  */
  providerId: ProviderId
  /**
   * User that is logged in.
  */
  user: Awaited<ReturnType<typeof findAuthWithUserBy>>['user']
  /**
   * OAuth flow data that was generated during the OAuth flow. This is only
   * available if the user logged in using OAuth.
  */
  oauth?: OAuthParams
  /**
   * Request object that can be used to access the incoming request.
  */
  req: ExpressRequest
} & InternalAuthHookParams

// PRIVATE API (server)
export type OAuthParams = {
  /**
   * Unique request ID that was generated during the OAuth flow.
  */
  uniqueRequestId: string
} & (
  | { providerName: 'google'; tokens: import('arctic').GoogleTokens } 
  | never
)
