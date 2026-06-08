
import type { ProviderConfig } from "wasp/auth/providers/types";
import { discord } from "wasp/server/auth";
import { mergeDefaultAndUserConfig } from "../oauth/config.js";
import { createOAuthProviderRouter } from "../oauth/handler.js";

import { discordUserSignupFields } from '../../../../../../../src/features/auth/providers/discord'
const _waspUserSignupFields = discordUserSignupFields
import { discordConfig } from '../../../../../../../src/features/auth/providers/discord'
const _waspUserDefinedConfigFn = discordConfig

const _waspConfig: ProviderConfig = {
    id: discord.id,
    displayName: discord.displayName,
    createRouter(provider) {
        const config = mergeDefaultAndUserConfig({
            scopes: ['identify'],
        }, _waspUserDefinedConfigFn);

        async function getDiscordProfile(accessToken: string): Promise<{
            providerProfile: unknown;
            providerUserId: string;
        }> {
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

        return createOAuthProviderRouter({
            provider,
            oAuthType: 'OAuth2',
            userSignupFields: _waspUserSignupFields,
            // Discord is used as a confidential client (we have a client secret),
            // so PKCE is not used and we pass `null` as the code verifier.
            getAuthorizationUrl: ({ state }) => discord.oAuthClient.createAuthorizationURL(state, null, config.scopes),
            getProviderTokens: ({ code }) => discord.oAuthClient.validateAuthorizationCode(code, null),
            getProviderInfo: (tokens) => getDiscordProfile(tokens.accessToken()),
        });
    },
}

export default _waspConfig;
