import { Router } from 'express'

import { handleRejection, redirect } from 'wasp/server/utils'
import { rethrowPossibleAuthError } from 'wasp/auth/utils'
import {
  type UserSignupFields,
  type ProviderConfig,
} from 'wasp/auth/providers/types'

import {
  type StateType,
  generateAndStoreOAuthState,
  validateAndGetOAuthState,
} from '../oauth/state.js'
import {
  finishOAuthFlowAndGetRedirectUri,
  handleOAuthErrorAndGetRedirectUri,
} from '../oauth/user.js'
import { callbackPath, loginPath } from './redirect.js'
import {
  onBeforeOAuthRedirectHook,
  onAfterOAuthTokenReceivedHook,
} from '../../hooks.js'

export function createOAuthProviderRouter<ST extends StateType>({
  provider,
  stateTypes,
  userSignupFields,
  getAuthorizationUrl,
  getProviderTokens,
  getProviderInfo,
}: {
  provider: ProviderConfig
  /*
    - State is used to validate the callback to ensure the user
      that requested the login is the same that is completing it.
    - It can include just the "state" or an extra "codeVerifier" for PKCE.
    - The state types used depend on the provider.
  */
  stateTypes: ST[]
  userSignupFields: UserSignupFields | undefined
  /*
    The function that returns the URL to redirect the user to the
    provider's login page.
  */
  getAuthorizationUrl: Parameters<typeof createOAuthLoginHandler<ST>>[2]
  /*
    The function that returns the access token and refresh token from the
    provider's callback.
  */
  getProviderTokens: Parameters<typeof createOAuthCallbackHandler<ST>>[3]
  /*
    The function that returns the user's profile and ID using the access
    token.
  */
  getProviderInfo: Parameters<typeof createOAuthCallbackHandler<ST>>[4]
}): Router {
  const router = Router()

  router.get(
    `/${loginPath}`,
    createOAuthLoginHandler(provider, stateTypes, getAuthorizationUrl),
  )

  router.get(
    `/${callbackPath}`,
    createOAuthCallbackHandler(
      provider,
      stateTypes,
      userSignupFields,
      getProviderTokens,
      getProviderInfo,
    ),
  )

  return router
}

function createOAuthLoginHandler<ST extends StateType>(
  provider: ProviderConfig,
  stateTypes: ST[],
  getAuthorizationUrl: (
    oAuthState: ReturnType<typeof generateAndStoreOAuthState<ST>>,
  ) => Promise<URL>,
) {
  return handleRejection(async (req, res) => {
    const oAuthState = generateAndStoreOAuthState(stateTypes, provider, res)
    let url = await getAuthorizationUrl(oAuthState)
    if (onBeforeOAuthRedirectHook) {
      url = (
        await onBeforeOAuthRedirectHook({
          req,
          url,
        })
      ).url
    }
    return redirect(res, url.toString())
  })
}

function createOAuthCallbackHandler<ST extends StateType>(
  provider: ProviderConfig,
  stateTypes: ST[],
  userSignupFields: UserSignupFields | undefined,
  getProviderTokens: (
    oAuthState: ReturnType<typeof validateAndGetOAuthState<ST>>,
  ) => Promise<{
    accessToken: string
  }>,
  getProviderInfo: ({ accessToken }: { accessToken: string }) => Promise<{
    providerUserId: string
    providerProfile: unknown
  }>,
) {
  return handleRejection(async (req, res) => {
    try {
      const oAuthState = validateAndGetOAuthState(stateTypes, provider, req)
      const { accessToken } = await getProviderTokens(oAuthState)
      if (onAfterOAuthTokenReceivedHook) {
        await onAfterOAuthTokenReceivedHook({
          req,
          accessToken,
        })
      }

      const { providerProfile, providerUserId } = await getProviderInfo({
        accessToken,
      })
      try {
        const redirectUri = await finishOAuthFlowAndGetRedirectUri(
          provider,
          providerProfile,
          providerUserId,
          userSignupFields,
          req,
        )
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
