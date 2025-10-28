
import type { ProviderConfig } from "wasp/auth/providers/types";
import { github } from "wasp/server/auth";
import { mergeDefaultAndUserConfig } from "../oauth/config.js";
import { createOAuthProviderRouter } from "../oauth/handler.js";

import { userSignupFields } from '../../../../../../../src/features/auth/providers/github.js'
const _waspUserSignupFields = userSignupFields
import { config } from '../../../../../../../src/features/auth/providers/github.js'
const _waspUserDefinedConfigFn = config

const _waspConfig: ProviderConfig = {
    id: github.id,
    displayName: github.displayName,
    createRouter(provider) {
        const config = mergeDefaultAndUserConfig({
            scopes: [],
        }, _waspUserDefinedConfigFn);

        async function getGithubProfile(accessToken: string): Promise<{
            providerProfile: unknown;
            providerUserId: string;
        }> {
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
                return scope === 'user' || scope === 'user:email';
            });
            if (isEmailAccessAllowed) {
                const emailsResponse = await fetch("https://api.github.com/user/emails", {
                    headers: {
                        Authorization: `Bearer ${accessToken}`,
                    },
                });
                const emails = (await emailsResponse.json()) as unknown[];
                providerProfile.emails = emails;
            }

            return { providerProfile, providerUserId: `${providerProfile.id}` };
        }

        return createOAuthProviderRouter({
            provider,
            oAuthType: 'OAuth2',
            userSignupFields: _waspUserSignupFields,
            getAuthorizationUrl: ({ state }) => github.oAuthClient.createAuthorizationURL(state, config),
            getProviderTokens: ({ code }) => github.oAuthClient.validateAuthorizationCode(code),
            getProviderInfo: ({ accessToken }) => getGithubProfile(accessToken),
        });
    },
}

export default _waspConfig;
