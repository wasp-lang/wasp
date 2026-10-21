import type { Discord } from "arctic";
import type { AuthHookTypes } from "../../../hooks.js";
import type { ProviderConfig } from "../../../providers.js";
import { mergeDefaultAndUserConfig } from "../config.js";
import {
  createOAuthProviderRouter,
  type OAuthDependencies,
} from "../handler.js";
import type { OAuthProviderAuthConfig, ProviderProfile } from "./common.js";

export function createDiscordAuthProvider<H extends AuthHookTypes>(
  {
    provider: discord,
    requiredScopes,
    userDefinedConfigFn,
    userSignupFields,
  }: OAuthProviderAuthConfig<Discord>,
  dependencies: OAuthDependencies<H>,
): ProviderConfig {
  return {
    id: discord.id,
    displayName: discord.displayName,
    createRouter(provider) {
      const config = mergeDefaultAndUserConfig(
        {
          scopes: requiredScopes,
        },
        userDefinedConfigFn,
      );

      async function getDiscordProfile(
        accessToken: string,
      ): Promise<ProviderProfile> {
        const response = await fetch("https://discord.com/api/users/@me", {
          headers: {
            Authorization: `Bearer ${accessToken}`,
          },
        });
        const providerProfile = (await response.json()) as {
          id?: string;
          email?: string;
          global_name?: string;
          avatar?: string;
        };

        if (!providerProfile.id) {
          throw new Error("Invalid profile");
        }

        if (providerProfile.avatar) {
          providerProfile.avatar = `https://cdn.discordapp.com/avatars/${providerProfile.id}/${providerProfile.avatar}.png`;
        }

        return { providerProfile, providerUserId: providerProfile.id };
      }

      return createOAuthProviderRouter(
        {
          provider,
          oAuthType: "OAuth2",
          userSignupFields,
          getAuthorizationUrl: ({ state }) =>
            discord.oAuthClient.createAuthorizationURL(state, config),
          getProviderTokens: ({ code }) =>
            discord.oAuthClient.validateAuthorizationCode(code),
          getProviderInfo: ({ accessToken }) => getDiscordProfile(accessToken),
        },
        dependencies,
      );
    },
  };
}
