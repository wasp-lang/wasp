import { Router } from 'express'

import { handleRejection, redirect } from 'wasp/server/utils'
import { rethrowPossibleAuthError } from 'wasp/auth/utils'
import {
  type UserSignupFields,
  type ProviderConfig,
} from 'wasp/auth/providers/types'

import {
  type OptionalStateType,
  generateAndStoreOAuthState,
  validateAndGetOAuthState,
} from '../oauth/state.js'
import {
  finishOAuthFlowAndGetRedirectUri,
  handleOAuthErrorAndGetRedirectUri,
} from '../oauth/user.js'
import { callbackPath, loginPath } from './redirect.js'
import { onBeforeOAuthRedirectHook } from '../../hooks.js'

export function createOAuthProviderRouter<OST extends OptionalStateType>({
  provider,
  optionalStateTypes,
  userSignupFields,
  getAuthorizationUrl,
  getProviderTokens,
  getProviderInfo,
}: {
  provider: ProviderConfig
  /*
    - State is used to validate the callback to ensure the user
      that requested the login is the same that is completing it.
    - It includes "state" and an optional "codeVerifier" for PKCE.
    - The state types used depend on the provider.
  */
  optionalStateTypes: OST[]
  userSignupFields: UserSignupFields | undefined
  /*
    The function that returns the URL to redirect the user to the
    provider's login page.
  */
  getAuthorizationUrl: Parameters<typeof createOAuthLoginHandler<OST>>[2]
  /*
    The function that returns the access token and refresh token from the
    provider's callback.
  */
  getProviderTokens: Parameters<typeof createOAuthCallbackHandler<OST>>[3]
  /*
    The function that returns the user's profile and ID using the access
    token.
  */
  getProviderInfo: Parameters<typeof createOAuthCallbackHandler<OST>>[4]
}): Router {
  const router = Router()

  router.get(
    `/${loginPath}`,
    createOAuthLoginHandler(provider, optionalStateTypes, getAuthorizationUrl),
  )

  router.get(
    `/${callbackPath}`,
    createOAuthCallbackHandler(
      provider,
      optionalStateTypes,
      userSignupFields,
      getProviderTokens,
      getProviderInfo,
    ),
  )

  return router
}

function createOAuthLoginHandler<OST extends OptionalStateType>(
  provider: ProviderConfig,
  optionalStateTypes: OST[],
  getAuthorizationUrl: (
    oAuthState: ReturnType<typeof generateAndStoreOAuthState<OST>>,
  ) => Promise<URL>,
) {
  return handleRejection(async (req, res) => {
    const oAuthState = generateAndStoreOAuthState(optionalStateTypes, provider, res)
    let url = await getAuthorizationUrl(oAuthState)
    if (onBeforeOAuthRedirectHook) {
      url = (
        await onBeforeOAuthRedirectHook({
          req,
          url,
          uniqueRequestId: oAuthState.state,
        })
      ).url
    }
    return redirect(res, url.toString())
  })
}

function createOAuthCallbackHandler<OST extends OptionalStateType>(
  provider: ProviderConfig,
  optionalStateTypes: OST[],
  userSignupFields: UserSignupFields | undefined,
  getProviderTokens: (
    oAuthState: ReturnType<typeof validateAndGetOAuthState<OST>>,
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
      const oAuthState = validateAndGetOAuthState(optionalStateTypes, provider, req)
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
