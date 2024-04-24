{{={= =}=}}
import { Keycloak } from "arctic";

import type { ProviderConfig } from "wasp/auth/providers/types";
import { getRedirectUriForCallback } from "../oauth/redirect.js";
import { ensureEnvVarsForProvider } from "../oauth/env.js";
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
    id: "{= providerId =}",
    displayName: "{= displayName =}",
    createRouter(provider) {
        const env = ensureEnvVarsForProvider(
            ["KEYCLOAK_REALM_URL", "KEYCLOAK_CLIENT_ID", "KEYCLOAK_CLIENT_SECRET"],
            provider
        );

        const keycloak = new Keycloak(
            env.KEYCLOAK_REALM_URL,
            env.KEYCLOAK_CLIENT_ID,
            env.KEYCLOAK_CLIENT_SECRET,
            getRedirectUriForCallback(provider.id).toString(),
        );

        const config = mergeDefaultAndUserConfig({
            scopes: {=& requiredScopes =},
        }, _waspUserDefinedConfigFn);

        async function getKeycloakProfile(accessToken: string): Promise<{
            providerProfile: unknown;
            providerUserId: string;
        }> {
            const userInfoEndpoint = `${env.KEYCLOAK_REALM_URL}/protocol/openid-connect/userinfo`;
            const response = await fetch(
                userInfoEndpoint,
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
            stateTypes: ['state', 'codeVerifier'],
            userSignupFields: _waspUserSignupFields,
            getAuthorizationUrl: ({ state, codeVerifier }) => keycloak.createAuthorizationURL(state, codeVerifier, config),
            getProviderTokens: ({ code, codeVerifier }) => keycloak.validateAuthorizationCode(code, codeVerifier),
            getProviderInfo: ({ accessToken }) => getKeycloakProfile(accessToken),
        });
    },
}

export default _waspConfig;
