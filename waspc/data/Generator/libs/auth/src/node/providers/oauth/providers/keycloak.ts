import type { Keycloak } from "arctic";
import type { AuthHookTypes } from "../../../hooks.js";
import type { ProviderConfig } from "../../../providers.js";
import { mergeDefaultAndUserConfig } from "../config.js";
import {
  createOAuthProviderRouter,
  type OAuthDependencies,
} from "../handler.js";
import type { OAuthProviderAuthConfig, ProviderProfile } from "./common.js";

export type KeycloakAuthConfig = OAuthProviderAuthConfig<Keycloak> & {
  /** The Keycloak realm URL, used to find the user info endpoint. */
  realmUrl: string;
};

export function createKeycloakAuthProvider<H extends AuthHookTypes>(
  {
    provider: keycloak,
    requiredScopes,
    userDefinedConfigFn,
    userSignupFields,
    realmUrl,
  }: KeycloakAuthConfig,
  dependencies: OAuthDependencies<H>,
): ProviderConfig {
  return {
    id: keycloak.id,
    displayName: keycloak.displayName,
    createRouter(provider) {
      const config = mergeDefaultAndUserConfig(
        {
          scopes: requiredScopes,
        },
        userDefinedConfigFn,
      );

      async function getKeycloakProfile(
        accessToken: string,
      ): Promise<ProviderProfile> {
        const userInfoEndpoint = `${realmUrl}/protocol/openid-connect/userinfo`;
        const response = await fetch(userInfoEndpoint, {
          headers: {
            Authorization: `Bearer ${accessToken}`,
          },
        });
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
            keycloak.oAuthClient.createAuthorizationURL(
              state,
              codeVerifier,
              config,
            ),
          getProviderTokens: ({ code, codeVerifier }) =>
            keycloak.oAuthClient.validateAuthorizationCode(code, codeVerifier),
          getProviderInfo: ({ accessToken }) => getKeycloakProfile(accessToken),
        },
        dependencies,
      );
    },
  };
}
