import type { MicrosoftEntraId } from "arctic";
import type { AuthHookTypes } from "../../../hooks.js";
import type { ProviderConfig } from "../../../providers.js";
import { mergeDefaultAndUserConfig } from "../config.js";
import {
  createOAuthProviderRouter,
  type OAuthDependencies,
} from "../handler.js";
import type { OAuthProviderAuthConfig, ProviderProfile } from "./common.js";

export function createMicrosoftAuthProvider<H extends AuthHookTypes>(
  {
    provider: microsoft,
    requiredScopes,
    userDefinedConfigFn,
    userSignupFields,
  }: OAuthProviderAuthConfig<MicrosoftEntraId>,
  dependencies: OAuthDependencies<H>,
): ProviderConfig {
  return {
    id: microsoft.id,
    displayName: microsoft.displayName,
    createRouter(provider) {
      const config = mergeDefaultAndUserConfig(
        {
          scopes: requiredScopes,
        },
        userDefinedConfigFn,
      );

      async function getMicrosoftProfile(
        accessToken: string,
      ): Promise<ProviderProfile> {
        const response = await fetch(
          "https://graph.microsoft.com/oidc/userinfo",
          {
            headers: {
              Authorization: `Bearer ${accessToken}`,
            },
          },
        );
        const providerProfile = (await response.json()) as {
          sub?: string;
        };

        if (!providerProfile.sub) {
          throw new Error("Invalid profile");
        }

        return { providerProfile, providerUserId: providerProfile.sub };
      }

      return createOAuthProviderRouter(
        {
          provider,
          oAuthType: "OAuth2WithPKCE",
          userSignupFields,
          getAuthorizationUrl: ({ state, codeVerifier }) =>
            microsoft.oAuthClient.createAuthorizationURL(
              state,
              codeVerifier,
              config,
            ),
          getProviderTokens: ({ code, codeVerifier }) =>
            microsoft.oAuthClient.validateAuthorizationCode(code, codeVerifier),
          getProviderInfo: ({ accessToken }) =>
            getMicrosoftProfile(accessToken),
        },
        dependencies,
      );
    },
  };
}
