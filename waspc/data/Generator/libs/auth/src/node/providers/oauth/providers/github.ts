import type { GitHub } from "arctic";
import type { AuthHookTypes } from "../../../hooks.js";
import type { ProviderConfig } from "../../../providers.js";
import { mergeDefaultAndUserConfig } from "../config.js";
import {
  createOAuthProviderRouter,
  type OAuthDependencies,
} from "../handler.js";
import type { OAuthProviderAuthConfig, ProviderProfile } from "./common.js";

export function createGitHubAuthProvider<H extends AuthHookTypes>(
  {
    provider: github,
    requiredScopes,
    userDefinedConfigFn,
    userSignupFields,
  }: OAuthProviderAuthConfig<GitHub>,
  dependencies: OAuthDependencies<H>,
): ProviderConfig {
  return {
    id: github.id,
    displayName: github.displayName,
    createRouter(provider) {
      const config = mergeDefaultAndUserConfig(
        {
          scopes: requiredScopes,
        },
        userDefinedConfigFn,
      );

      async function getGithubProfile(
        accessToken: string,
      ): Promise<ProviderProfile> {
        const response = await fetch("https://api.github.com/user", {
          headers: {
            Authorization: `Bearer ${accessToken}`,
          },
        });
        const providerProfile = (await response.json()) as {
          id?: string;
          emails?: unknown[];
        };

        if (!providerProfile.id) {
          throw new Error("Invalid profile");
        }

        const scopes = config.scopes as string[];
        // Using the logic from https://github.com/cfsghost/passport-github/blob/master/lib/strategy.js#L118C24-L120C10
        const isEmailAccessAllowed = scopes.some((scope) => {
          return scope === "user" || scope === "user:email";
        });
        if (isEmailAccessAllowed) {
          const emailsResponse = await fetch(
            "https://api.github.com/user/emails",
            {
              headers: {
                Authorization: `Bearer ${accessToken}`,
              },
            },
          );
          const emails = (await emailsResponse.json()) as unknown[];
          providerProfile.emails = emails;
        }

        return { providerProfile, providerUserId: `${providerProfile.id}` };
      }

      return createOAuthProviderRouter(
        {
          provider,
          oAuthType: "OAuth2",
          userSignupFields,
          getAuthorizationUrl: ({ state }) =>
            github.oAuthClient.createAuthorizationURL(state, config),
          getProviderTokens: ({ code }) =>
            github.oAuthClient.validateAuthorizationCode(code),
          getProviderInfo: ({ accessToken }) => getGithubProfile(accessToken),
        },
        dependencies,
      );
    },
  };
}
