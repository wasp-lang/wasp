import { defineHandler, redirect } from "@wasp.sh/lib-sdk-core/node";
import { Router } from "express";
import type { AuthHookTypes, AuthHooks } from "../../hooks.js";
import type { AuthIdentities, UserSignupFields } from "../../identities.js";
import type { ProviderConfig } from "../../providers.js";
import type { OneTimeCodeStore } from "./oneTimeCode.js";
import type { OAuthRedirects } from "./redirects.js";
import {
  generateAndStoreOAuthState,
  validateAndGetOAuthState,
  type OAuthStateFor,
  type OAuthStateWithCodeFor,
  type OAuthType,
} from "./state.js";
import { finishOAuthFlowAndGetRedirectUri } from "./user.js";

/** What the OAuth flows need from the framework and the app. */
export type OAuthDependencies<H extends AuthHookTypes> = {
  identities: AuthIdentities<H["entities"]>;
  hooks: AuthHooks<H>;
  redirects: OAuthRedirects;
  tokenStore: OneTimeCodeStore;
  /** Whether the server runs in development (decides whether OAuth cookies are `secure`). */
  isDevelopment: boolean;
};

export function createOAuthProviderRouter<
  H extends AuthHookTypes,
  OT extends OAuthType,
  Tokens = never,
>(
  {
    provider,
    oAuthType,
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
    oAuthType: OT;
    userSignupFields: UserSignupFields | undefined;
    /*
    The function that returns the URL to redirect the user to the
    provider's login page.
  */
    getAuthorizationUrl: (oAuthState: OAuthStateFor<OT>) => Promise<URL>;
    /*
    The function that returns the access token and refresh token from the
    provider's callback.
  */
    getProviderTokens: (
      oAuthState: OAuthStateWithCodeFor<OT>,
    ) => Promise<Tokens>;
    /*
    The function that returns the user's profile and ID using the access
    token.
  */
    getProviderInfo: (tokens: Tokens) => Promise<{
      providerUserId: string;
      providerProfile: unknown;
    }>;
  },
  dependencies: OAuthDependencies<H>,
): Router {
  const { identities, hooks, redirects, isDevelopment } = dependencies;
  const router = Router();

  router.get(
    `/${redirects.loginPath}`,
    defineHandler(async (req, res) => {
      const oAuthState = generateAndStoreOAuthState({
        oAuthType,
        provider,
        res,
        isDevelopment,
      });
      const redirectUrl = await getAuthorizationUrl(oAuthState);
      const { url: redirectUrlAfterHook } =
        await hooks.onBeforeOAuthRedirectHook({
          req,
          url: redirectUrl,
          oauth: { uniqueRequestId: oAuthState.state },
        });
      redirect(res, redirectUrlAfterHook.toString());
    }),
  );

  router.get(
    `/${redirects.callbackPath}`,
    defineHandler(async (req, res) => {
      try {
        const oAuthState = validateAndGetOAuthState({
          oAuthType,
          provider,
          req,
        });
        const tokens = await getProviderTokens(oAuthState);

        const { providerProfile, providerUserId } =
          await getProviderInfo(tokens);
        try {
          const redirectUri = await finishOAuthFlowAndGetRedirectUri(
            {
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
              } as unknown as H["oauthData"],
            },
            dependencies,
          );
          // Redirect to the client with the one time code
          redirect(res, redirectUri.toString());
        } catch (e) {
          identities.rethrowPossibleAuthError(e);
        }
      } catch (e) {
        console.error(e);
        const redirectUri = redirects.handleOAuthErrorAndGetRedirectUri(e);
        // Redirect to the client with the error
        redirect(res, redirectUri.toString());
      }
    }),
  );

  return router;
}
