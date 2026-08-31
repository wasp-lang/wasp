import { Router } from "express";

import {
  type ProviderConfig,
  type UserSignupFields,
} from "wasp/auth/providers/types";
import {
  OAuthData,
  completeOAuthLogin,
  getOAuthLoginErrorRedirectUrl,
  providerCallbackPath,
  providerLoginPath,
} from "wasp/server/auth";
import { rethrowPossibleAuthError } from "wasp/server/auth/utils";
import { defineHandler, redirect } from "wasp/server/utils";
import { onBeforeOAuthRedirectHook } from "../../hooks.js";
import {
  type OAuthCallbackStateFor,
  type OAuthStateFor,
  type OAuthType,
  generateAndStoreOAuthState,
  validateAndGetOAuthState,
} from "../oauth/state.js";
import { resolveOAuthIdentity } from "../oauth/user.js";

export function createOAuthProviderRouter<
  OT extends OAuthType,
  Tokens extends OAuthData["tokens"] = never,
>({
  provider,
  oauthType,
  userSignupFields,
  getAuthorizationUrl,
  getProviderTokens,
  getProviderInfo,
}: {
  provider: ProviderConfig;
  /*
    - OAuth state is used to validate the callback to ensure the user
      that requested the login is the same that is completing it.
    - It includes "state" and an optional "codeVerifier" for PKCE.
  */
  oauthType: OT;
  userSignupFields: UserSignupFields | undefined;
  /*
    The function that returns the URL to redirect the user to the
    provider's login page.
  */
  getAuthorizationUrl: (oauthState: OAuthStateFor<OT>) => Promise<URL>;
  /*
    The function that returns the access token and refresh token from the
    provider's callback.
  */
  getProviderTokens: (oauthState: OAuthCallbackStateFor<OT>) => Promise<Tokens>;
  /*
    The function that returns the user's profile and ID using the access
    token.
  */
  getProviderInfo: (tokens: Tokens) => Promise<{
    providerUserId: string;
    providerProfile: unknown;
  }>;
}): Router {
  const router = Router();

  router.get(
    `/${providerLoginPath}`,
    defineHandler(async (req, res) => {
      const oauthState = generateAndStoreOAuthState({
        oauthType,
        provider,
        res,
      });
      const redirectUrl = await getAuthorizationUrl(oauthState);
      const { url: redirectUrlAfterHook } = await onBeforeOAuthRedirectHook({
        req,
        url: redirectUrl,
        oauth: { uniqueRequestId: oauthState.state },
      });
      redirect(res, redirectUrlAfterHook.toString());
    }),
  );

  router.get(
    `/${providerCallbackPath}`,
    defineHandler(async (req, res) => {
      try {
        const oauthState = validateAndGetOAuthState({
          oauthType,
          provider,
          req,
        });
        const tokens = await getProviderTokens(oauthState);

        const { providerProfile, providerUserId } =
          await getProviderInfo(tokens);
        try {
          const authId = await resolveOAuthIdentity({
            provider,
            providerProfile,
            providerUserId,
            userSignupFields,
            req,
            oauth: {
              uniqueRequestId: oauthState.state,
              // OAuth params are built as a discriminated union
              // of provider names and their respective tokens.
              // We are using a generic ProviderConfig and tokens type
              // is inferred from the getProviderTokens function.
              // Instead of building complex TS machinery to ensure that
              // the providerName and tokens match, we are using any here.
              providerName: provider.id as any,
              tokens,
            },
          });
          const loginResultUrl = await completeOAuthLogin({
            authId,
            response: res,
          });
          redirect(res, loginResultUrl.toString());
        } catch (e) {
          rethrowPossibleAuthError(e);
        }
      } catch (e) {
        console.error(e);
        const loginErrorUrl = getOAuthLoginErrorRedirectUrl(e);
        redirect(res, loginErrorUrl.toString());
      }
    }),
  );

  return router;
}
