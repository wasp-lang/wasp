{{={= =}=}}
import { GitHub } from "arctic";

import type { ProviderConfig } from "wasp/auth/providers/types";
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
            ["GITHUB_CLIENT_ID", "GITHUB_CLIENT_SECRET"],
            provider
        );

        const github = new GitHub(
            env.GITHUB_CLIENT_ID,
            env.GITHUB_CLIENT_SECRET,
        );

        const config = mergeDefaultAndUserConfig({
            scopes: {=& requiredScopes =},
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
            stateTypes: ['state'],
            userSignupFields: _waspUserSignupFields,
            getAuthorizationUrl: ({ state }) => github.createAuthorizationURL(state, config),
            getProviderTokens: ({ code }) => github.validateAuthorizationCode(code),
            getProviderInfo: ({ accessToken }) => getGithubProfile(accessToken),
        });
    },
}

export default _waspConfig;
