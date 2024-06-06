import { Router } from 'express'

import { handleRejection, redirect } from 'wasp/server/utils'
import { rethrowPossibleAuthError } from 'wasp/auth/utils'
import {
  type UserSignupFields,
  type ProviderConfig,
} from 'wasp/auth/providers/types'

import {
  type OAuthState,
  generateAndStoreOAuthState,
  validateAndGetOAuthState,
} from '../oauth/state.js'
import {
  finishOAuthFlowAndGetRedirectUri,
  handleOAuthErrorAndGetRedirectUri,
} from '../oauth/user.js'
import { callbackPath, loginPath } from './redirect.js'
import { onBeforeOAuthRedirectHook } from '../../hooks.js'

export function createOAuthProviderRouter<IsCodeVerifierUsed extends boolean>({
  provider,
  isCodeVerifierUsed,
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
  isCodeVerifierUsed: IsCodeVerifierUsed,
  userSignupFields: UserSignupFields | undefined
  /*
    The function that returns the URL to redirect the user to the
    provider's login page.
  */
  getAuthorizationUrl: Parameters<typeof createOAuthLoginHandler<IsCodeVerifierUsed>>[0]['getAuthorizationUrl']
  /*
    The function that returns the access token and refresh token from the
    provider's callback.
  */
  getProviderTokens: Parameters<typeof createOAuthCallbackHandler<IsCodeVerifierUsed>>[0]['getProviderTokens']
  /*
    The function that returns the user's profile and ID using the access
    token.
  */
  getProviderInfo: Parameters<typeof createOAuthCallbackHandler<IsCodeVerifierUsed>>[0]['getProviderInfo']
}): Router {
  const router = Router()

  router.get(
    `/${loginPath}`,
    createOAuthLoginHandler({ provider, isCodeVerifierUsed, getAuthorizationUrl }),
  )

  router.get(
    `/${callbackPath}`,
    createOAuthCallbackHandler({
      provider,
      isCodeVerifierUsed,
      userSignupFields,
      getProviderTokens,
      getProviderInfo,
    }),
  )

  return router
}

function createOAuthLoginHandler<IsCodeVerifierUsed extends boolean>({
  provider,
  isCodeVerifierUsed,
  getAuthorizationUrl,
}: {
  provider: ProviderConfig,
  isCodeVerifierUsed: IsCodeVerifierUsed,
  getAuthorizationUrl: (
    oAuthState: OAuthState<IsCodeVerifierUsed>,
  ) => Promise<URL>,
}) {
  return handleRejection(async (req, res) => {
    const oAuthState = generateAndStoreOAuthState(isCodeVerifierUsed, provider, res)
    const redirectUrl = await getAuthorizationUrl(oAuthState)
    const { url: redirectUrlAfterHook } = await onBeforeOAuthRedirectHook({
      req,
      url: redirectUrl,
      uniqueRequestId: oAuthState.state,
    })
    return redirect(res, redirectUrlAfterHook.toString())
  })
}

function createOAuthCallbackHandler<IsCodeVerifierUsed extends boolean>({
  provider,
  isCodeVerifierUsed,
  userSignupFields,
  getProviderTokens,
  getProviderInfo,
}: {
  provider: ProviderConfig,
  isCodeVerifierUsed: IsCodeVerifierUsed,
  userSignupFields: UserSignupFields | undefined,
  getProviderTokens: (
    oAuthState: OAuthState<IsCodeVerifierUsed>,
  ) => Promise<{
    accessToken: string
  }>,
  getProviderInfo: ({ accessToken }: { accessToken: string }) => Promise<{
    providerUserId: string
    providerProfile: unknown
  }>,
}) {
  return handleRejection(async (req, res) => {
    try {
      const oAuthState = validateAndGetOAuthState(isCodeVerifierUsed, provider, req)
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
  })
}
