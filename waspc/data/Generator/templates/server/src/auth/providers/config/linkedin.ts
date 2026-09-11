{{={= =}=}}
import type { ProviderConfig } from "wasp/auth/providers/types";
import { linkedin } from "wasp/server/auth";

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
    id: linkedin.id,
    displayName: linkedin.displayName,
    createRouter(provider) {
        const config = mergeDefaultAndUserConfig({
            scopes: {=& requiredScopes =},
        }, _waspUserDefinedConfigFn);

        async function getLinkedInProfile(accessToken: string): Promise<{
            providerProfile: unknown;
            providerUserId: string;
        }> {
            const response = await fetch("https://api.linkedin.com/v2/userinfo", {
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

        return createOAuthProviderRouter({
            provider,
            oAuthType: 'OAuth2',
            userSignupFields: _waspUserSignupFields,
            getAuthorizationUrl: ({ state }) => linkedin.oAuthClient.createAuthorizationURL(state, config),
            getProviderTokens: ({ code }) => linkedin.oAuthClient.validateAuthorizationCode(code),
            getProviderInfo: ({ accessToken }) => getLinkedInProfile(accessToken),
        });
    },
}

export default _waspConfig;
