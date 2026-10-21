import type { Slack } from "arctic";
import type { AuthHookTypes } from "../../../hooks.js";
import type { ProviderConfig } from "../../../providers.js";
import { mergeDefaultAndUserConfig } from "../config.js";
import {
  createOAuthProviderRouter,
  type OAuthDependencies,
} from "../handler.js";
import type { OAuthProviderAuthConfig, ProviderProfile } from "./common.js";

export function createSlackAuthProvider<H extends AuthHookTypes>(
  {
    provider: slack,
    requiredScopes,
    userDefinedConfigFn,
    userSignupFields,
  }: OAuthProviderAuthConfig<Slack>,
  dependencies: OAuthDependencies<H>,
): ProviderConfig {
  return {
    id: slack.id,
    displayName: slack.displayName,
    createRouter(provider) {
      const config = mergeDefaultAndUserConfig(
        {
          scopes: requiredScopes,
        },
        userDefinedConfigFn,
      );

      async function getSlackProfile(
        accessToken: string,
      ): Promise<ProviderProfile> {
        const response = await fetch(
          "https://slack.com/api/openid.connect.userInfo",
          {
            headers: {
              Authorization: `Bearer ${accessToken}`,
            },
          },
        );

        const providerProfile = (await response.json()) as {
          sub?: string;
          email?: string;
          name?: string;
          picture?: string;
        };

        if (!providerProfile.sub) {
          throw new Error("Invalid profile");
        }

        return { providerProfile, providerUserId: providerProfile.sub };
      }

      return createOAuthProviderRouter(
        {
          provider,
          oAuthType: "OAuth2",
          userSignupFields,
          getAuthorizationUrl: ({ state }) =>
            slack.oAuthClient.createAuthorizationURL(state, config),
          getProviderTokens: ({ code }) =>
            slack.oAuthClient.validateAuthorizationCode(code),
          getProviderInfo: ({ accessToken }) => getSlackProfile(accessToken),
        },
        dependencies,
      );
    },
  };
}
