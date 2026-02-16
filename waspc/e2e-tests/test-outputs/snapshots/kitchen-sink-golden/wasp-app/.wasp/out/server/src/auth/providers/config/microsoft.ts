import type { ProviderConfig } from "wasp/auth/providers/types";
import { microsoft } from "wasp/server/auth";

import { mergeDefaultAndUserConfig } from "../oauth/config.js";
import { createOAuthProviderRouter } from "../oauth/handler.js";

import { userSignupFields } from '../../../../../../../src/features/auth/providers/microsoft'
const _waspUserSignupFields = userSignupFields
import { config } from '../../../../../../../src/features/auth/providers/microsoft'
const _waspUserDefinedConfigFn = config

const _waspConfig: ProviderConfig = {
    id: microsoft.id,
    displayName: microsoft.displayName,
    createRouter(provider) {
        const config = mergeDefaultAndUserConfig({
            scopes: ['openid', 'profile', 'email'],
        }, _waspUserDefinedConfigFn);

        async function getMicrosoftProfile(accessToken: string): Promise<{
            providerProfile: unknown;
            providerUserId: string;
        }> {
            const response = await fetch(
                "https://graph.microsoft.com/oidc/userinfo",
                {
                    headers: {
                        Authorization: `Bearer ${accessToken}`,
                    },
                }
            );
            const providerProfile = (await response.json()) as {
                sub?: string;
            };

            if (!providerProfile.sub) {
                throw new Error("Invalid profile");
            }

            return { providerProfile, providerUserId: providerProfile.sub };
        }

        return createOAuthProviderRouter({
            provider,
            oAuthType: 'OAuth2WithPKCE',
            userSignupFields: _waspUserSignupFields,
            getAuthorizationUrl: ({ state, codeVerifier }) => microsoft.oAuthClient.createAuthorizationURL(state, codeVerifier, config),
            getProviderTokens: ({ code, codeVerifier }) => microsoft.oAuthClient.validateAuthorizationCode(code, codeVerifier),
            getProviderInfo: ({ accessToken }) => getMicrosoftProfile(accessToken),
        });
    },
}

export default _waspConfig;
