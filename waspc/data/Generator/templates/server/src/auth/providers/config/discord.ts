{{={= =}=}}

import type { ProviderConfig } from "wasp/auth/providers/types";
import { discord } from "wasp/server/auth";
import { mergeDefaultAndUserConfig } from "../oauth/config.js";
import { createOAuthProviderRouter } from "../oauth/handler.js";

{=# userSignupFields.isDefined =}
{=& userSignupFields.importStatement =}
const _waspUserSignupFields = {= userSignupFields.importIdentifier =}
{=/ userSignupFields.isDefined =}
{=^ userSignupFields.isDefined =}
const _waspUserSignupFields = undefined
{=/ userSignupFields.isDefined =}
{=# configFn.isDefined =}
{=& configFn.importStatement =}
const _waspUserDefinedConfigFn = {= configFn.importIdentifier =}
{=/ configFn.isDefined =}
{=^ configFn.isDefined =}
const _waspUserDefinedConfigFn = undefined
{=/ configFn.isDefined =}

const _waspConfig: ProviderConfig = {
    id: discord.id,
    displayName: discord.displayName,
    createRouter(provider) {
        const config = mergeDefaultAndUserConfig({
            scopes: {=& requiredScopes =},
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
            getAuthorizationUrl: ({ state }) => discord.oAuthClient.createAuthorizationURL(state, config),
            getProviderTokens: ({ code }) => discord.oAuthClient.validateAuthorizationCode(code),
            getProviderInfo: ({ accessToken }) => getDiscordProfile(accessToken),
        });
    },
}

export default _waspConfig;
