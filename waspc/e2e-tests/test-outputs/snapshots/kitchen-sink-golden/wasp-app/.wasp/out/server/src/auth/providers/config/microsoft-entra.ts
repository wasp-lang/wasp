import type { ProviderConfig } from "wasp/auth/providers/types";
import { microsoftEntra } from "wasp/server/auth";

import { mergeDefaultAndUserConfig } from "../oauth/config.js";
import { createOAuthProviderRouter } from "../oauth/handler.js";

import { userSignupFields } from '../../../../../../../src/features/auth/providers/microsoftEntra'
const _waspUserSignupFields = userSignupFields
import { config } from '../../../../../../../src/features/auth/providers/microsoftEntra'
const _waspUserDefinedConfigFn = config

const _waspConfig: ProviderConfig = {
    id: microsoftEntra.id,
    displayName: microsoftEntra.displayName,
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
            const profile = (await response.json()) as {
                sub?: string;
                givenname?: string;
                familyname?: string;
            };

            if (!profile.sub) {
                throw new Error("Invalid profile");
            }

            // Microsoft uses non-standard field names
            const providerProfile = {
                ...profile,
                given_name: profile.givenname,
                family_name: profile.familyname,
            };

            return { providerProfile, providerUserId: profile.sub };
        }

        return createOAuthProviderRouter({
            provider,
            oAuthType: 'OAuth2WithPKCE',
            userSignupFields: _waspUserSignupFields,
            getAuthorizationUrl: ({ state, codeVerifier }) => microsoftEntra.oAuthClient.createAuthorizationURL(state, codeVerifier, config),
            getProviderTokens: ({ code, codeVerifier }) => microsoftEntra.oAuthClient.validateAuthorizationCode(code, codeVerifier),
            getProviderInfo: ({ accessToken }) => getMicrosoftProfile(accessToken),
        });
    },
}

export default _waspConfig;
