import type { Google } from "arctic";
import type { AuthHookTypes } from "../../../hooks.js";
import type { ProviderConfig } from "../../../providers.js";
import { mergeDefaultAndUserConfig } from "../config.js";
import {
  createOAuthProviderRouter,
  type OAuthDependencies,
} from "../handler.js";
import type { OAuthProviderAuthConfig, ProviderProfile } from "./common.js";

export function createGoogleAuthProvider<H extends AuthHookTypes>(
  {
    provider: google,
    requiredScopes,
    userDefinedConfigFn,
    userSignupFields,
  }: OAuthProviderAuthConfig<Google>,
  dependencies: OAuthDependencies<H>,
): ProviderConfig {
  return {
    id: google.id,
    displayName: google.displayName,
    createRouter(provider) {
      const config = mergeDefaultAndUserConfig(
        {
          scopes: requiredScopes,
        },
        userDefinedConfigFn,
      );

      async function getGoogleProfile(
        accessToken: string,
      ): Promise<ProviderProfile> {
        const response = await fetch(
          "https://openidconnect.googleapis.com/v1/userinfo",
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
            google.oAuthClient.createAuthorizationURL(
              state,
              codeVerifier,
              config,
            ),
          getProviderTokens: ({ code, codeVerifier }) =>
            google.oAuthClient.validateAuthorizationCode(code, codeVerifier),
          getProviderInfo: ({ accessToken }) => getGoogleProfile(accessToken),
        },
        dependencies,
      );
    },
  };
}
