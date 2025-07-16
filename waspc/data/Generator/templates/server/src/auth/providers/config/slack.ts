{{={= =}=}}
import type { ProviderConfig } from "wasp/auth/providers/types";
import { slack } from "wasp/server/auth";
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
    id: slack.id,
    displayName: slack.displayName,
    createRouter(provider) {
        const config = mergeDefaultAndUserConfig({
            scopes: {=& requiredScopes =},
        }, _waspUserDefinedConfigFn);

        async function getSlackProfile(accessToken: string): Promise<{
            providerProfile: unknown;
            providerUserId: string;
        }> {
            const response = await fetch("https://slack.com/api/openid.connect.userInfo", {
                headers: {
                  Authorization: `Bearer ${accessToken}`,
                },
              });

              const providerProfile = (await response.json()) as {
                sub?: string;
                email?: string;
                name?: string;
                picture?: string;
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
            getAuthorizationUrl: ({ state }) => slack.oAuthClient.createAuthorizationURL(state, config),
            getProviderTokens: ({ code }) => slack.oAuthClient.validateAuthorizationCode(code),
            getProviderInfo: ({ accessToken }) => getSlackProfile(accessToken),
        });
    },
}

export default _waspConfig;
