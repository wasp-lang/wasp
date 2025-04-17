import { Router } from 'express'

import {
    type ProviderConfig,
    type UserSignupFields,
} from 'wasp/auth/providers/types'
import { rethrowPossibleAuthError } from 'wasp/auth/utils'
import {
    OAuthData,
    callbackPath,
    handleOAuthErrorAndGetRedirectUri,
    loginPath,
} from 'wasp/server/auth'
import { defineHandler, redirect } from 'wasp/server/utils'
import { onBeforeOAuthRedirectHook } from '../../hooks.js'
import {
    type OAuthStateFor,
    type OAuthStateWithCodeFor,
    type OAuthType,
    generateAndStoreOAuthState,
    validateAndGetOAuthState,
} from '../oauth/state.js'
import { finishOAuthFlowAndGetRedirectUri } from '../oauth/user.js'

export function createOAuthProviderRouter<OT extends OAuthType, Tokens extends OAuthData['tokens'] = never>({
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
  ) => Promise<Tokens>
  /*
    The function that returns the user's profile and ID using the access
    token.
  */
  getProviderInfo: (tokens: Tokens) => Promise<{
    providerUserId: string
    providerProfile: unknown
  }>
}): Router {
  const router = Router()

  router.get(
    `/${loginPath}`,
    defineHandler(async (req, res) => {
      const oAuthState = generateAndStoreOAuthState({
        oAuthType,
        provider,
        res,
      })
      const redirectUrl = await getAuthorizationUrl(oAuthState)
      const { url: redirectUrlAfterHook } = await onBeforeOAuthRedirectHook({
        req,
        url: redirectUrl,
        oauth: { uniqueRequestId: oAuthState.state }
      })
      return redirect(res, redirectUrlAfterHook.toString())
    }),
  )

  router.get(
    `/${callbackPath}`,
    defineHandler(async (req, res) => {
      try {
        const oAuthState = validateAndGetOAuthState({
          oAuthType,
          provider,
          req,
        })
        const tokens = await getProviderTokens(oAuthState)

        const { providerProfile, providerUserId } = await getProviderInfo(tokens)
        try {
          const redirectUri = await finishOAuthFlowAndGetRedirectUri({
            provider,
            providerProfile,
            providerUserId,
            userSignupFields,
            req,
            oauth: {
              uniqueRequestId: oAuthState.state,
              // OAuth params are built as a discriminated union
              // of provider names and their respective tokens.
              // We are using a generic ProviderConfig and tokens type
              // is inferred from the getProviderTokens function.
              // Instead of building complex TS machinery to ensure that
              // the providerName and tokens match, we are using any here.
              providerName: provider.id as any,
              tokens,
            },
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
