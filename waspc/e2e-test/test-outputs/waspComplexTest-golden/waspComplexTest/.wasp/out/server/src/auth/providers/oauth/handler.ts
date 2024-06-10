import { Router } from 'express'

import { handleRejection, redirect } from 'wasp/server/utils'
import { rethrowPossibleAuthError } from 'wasp/auth/utils'
import {
  type UserSignupFields,
  type ProviderConfig,
} from 'wasp/auth/providers/types'

import {
  type OAuthType,
  type OAuthStateFor,
  type OAuthStateWithCodeFor,
  generateAndStoreOAuthState,
  validateAndGetOAuthState,
} from '../oauth/state.js'
import { finishOAuthFlowAndGetRedirectUri } from '../oauth/user.js'
import {
  callbackPath,
  loginPath,
  handleOAuthErrorAndGetRedirectUri,
} from './redirect.js'
import { onBeforeOAuthRedirectHook } from '../../hooks.js'

export function createOAuthProviderRouter<OT extends OAuthType>({
  provider,
  oAuthType,
  userSignupFields,
  getAuthorizationUrl,
  getProviderTokens,
  getProviderInfo,
}: {
  provider: ProviderConfig
  /*
    - OAuth state is used to validate the callback to ensure the user
      that requested the login is the same that is completing it.
    - It includes "state" and an optional "codeVerifier" for PKCE.
  */
  oAuthType: OT
  userSignupFields: UserSignupFields | undefined
  /*
    The function that returns the URL to redirect the user to the
    provider's login page.
  */
  getAuthorizationUrl: (
    oAuthState: OAuthStateFor<OT>,
  ) => Promise<URL>
  /*
    The function that returns the access token and refresh token from the
    provider's callback.
  */
  getProviderTokens: (
    oAuthState: OAuthStateWithCodeFor<OT>,
  ) => Promise<{
    accessToken: string
  }>
  /*
    The function that returns the user's profile and ID using the access
    token.
  */
  getProviderInfo: ({ accessToken }: { accessToken: string }) => Promise<{
    providerUserId: string
    providerProfile: unknown
  }>
}): Router {
  const router = Router()

  router.get(
    `/${loginPath}`,
    handleRejection(async (req, res) => {
      const oAuthState = generateAndStoreOAuthState({
        oAuthType,
        provider,
        res,
      })
      const redirectUrl = await getAuthorizationUrl(oAuthState)
      const { url: redirectUrlAfterHook } = await onBeforeOAuthRedirectHook({
        req,
        url: redirectUrl,
        uniqueRequestId: oAuthState.state,
      })
      return redirect(res, redirectUrlAfterHook.toString())
    }),
  )

  router.get(
    `/${callbackPath}`,
    handleRejection(async (req, res) => {
      try {
        const oAuthState = validateAndGetOAuthState({
          oAuthType,
          provider,
          req,
        })
        const { accessToken } = await getProviderTokens(oAuthState)
  
        const { providerProfile, providerUserId } = await getProviderInfo({
          accessToken,
        })
        try {
          const redirectUri = await finishOAuthFlowAndGetRedirectUri({
            provider,
            providerProfile,
            providerUserId,
            userSignupFields,
            req,
            accessToken,
            oAuthState,
          })
          // Redirect to the client with the one time code
          return redirect(res, redirectUri.toString())
        } catch (e) {
          rethrowPossibleAuthError(e)
        }
      } catch (e) {
        console.error(e)
        const redirectUri = handleOAuthErrorAndGetRedirectUri(e)
        // Redirect to the client with the error
        return redirect(res, redirectUri.toString())
      }
    }),
  )

  return router
}
