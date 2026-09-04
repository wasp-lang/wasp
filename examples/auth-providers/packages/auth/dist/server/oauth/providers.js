import { Discord, GitHub, Google, Keycloak, MicrosoftEntraId, Slack, } from "arctic";
async function fetchProfile(url, accessToken) {
    const response = await fetch(url, {
        headers: { Authorization: `Bearer ${accessToken}` },
    });
    return (await response.json());
}
function requireEnv(runtime, name) {
    const value = runtime.env[name];
    if (value === undefined) {
        throw new Error(`${name} is required by Wasp's auth.`);
    }
    return value;
}
export function makeOAuthProvider(runtime, id, callbackUrl) {
    switch (id) {
        case "google": {
            const client = new Google(requireEnv(runtime, "GOOGLE_CLIENT_ID"), requireEnv(runtime, "GOOGLE_CLIENT_SECRET"), callbackUrl);
            return {
                id,
                displayName: "Google",
                oAuthType: "OAuth2WithPKCE",
                getAuthorizationUrl: ({ state, codeVerifier }, config) => client.createAuthorizationURL(state, codeVerifier, config),
                getProviderTokens: ({ code, codeVerifier }) => client.validateAuthorizationCode(code, codeVerifier),
                getProviderInfo: async ({ accessToken }) => {
                    const providerProfile = await fetchProfile("https://openidconnect.googleapis.com/v1/userinfo", accessToken);
                    if (!providerProfile.sub)
                        throw new Error("Invalid profile");
                    return {
                        providerProfile,
                        providerUserId: providerProfile.sub,
                    };
                },
            };
        }
        case "keycloak": {
            const realmUrl = requireEnv(runtime, "KEYCLOAK_REALM_URL");
            const client = new Keycloak(realmUrl, requireEnv(runtime, "KEYCLOAK_CLIENT_ID"), requireEnv(runtime, "KEYCLOAK_CLIENT_SECRET"), callbackUrl);
            return {
                id,
                displayName: "Keycloak",
                oAuthType: "OAuth2WithPKCE",
                getAuthorizationUrl: ({ state, codeVerifier }, config) => client.createAuthorizationURL(state, codeVerifier, config),
                getProviderTokens: ({ code, codeVerifier }) => client.validateAuthorizationCode(code, codeVerifier),
                getProviderInfo: async ({ accessToken }) => {
                    const providerProfile = await fetchProfile(`${realmUrl}/protocol/openid-connect/userinfo`, accessToken);
                    if (!providerProfile.sub)
                        throw new Error("Invalid profile");
                    return {
                        providerProfile,
                        providerUserId: providerProfile.sub,
                    };
                },
            };
        }
        case "microsoft": {
            const client = new MicrosoftEntraId(requireEnv(runtime, "MICROSOFT_TENANT_ID"), requireEnv(runtime, "MICROSOFT_CLIENT_ID"), requireEnv(runtime, "MICROSOFT_CLIENT_SECRET"), callbackUrl);
            return {
                id,
                displayName: "Microsoft",
                oAuthType: "OAuth2WithPKCE",
                getAuthorizationUrl: ({ state, codeVerifier }, config) => client.createAuthorizationURL(state, codeVerifier, config),
                getProviderTokens: ({ code, codeVerifier }) => client.validateAuthorizationCode(code, codeVerifier),
                getProviderInfo: async ({ accessToken }) => {
                    const providerProfile = await fetchProfile("https://graph.microsoft.com/oidc/userinfo", accessToken);
                    if (!providerProfile.sub)
                        throw new Error("Invalid profile");
                    return {
                        providerProfile,
                        providerUserId: providerProfile.sub,
                    };
                },
            };
        }
        case "github": {
            const client = new GitHub(requireEnv(runtime, "GITHUB_CLIENT_ID"), requireEnv(runtime, "GITHUB_CLIENT_SECRET"));
            return {
                id,
                displayName: "GitHub",
                oAuthType: "OAuth2",
                getAuthorizationUrl: ({ state }, config) => client.createAuthorizationURL(state, config),
                getProviderTokens: ({ code }) => client.validateAuthorizationCode(code),
                getProviderInfo: async ({ accessToken }, config) => {
                    const providerProfile = await fetchProfile("https://api.github.com/user", accessToken);
                    if (!providerProfile.id)
                        throw new Error("Invalid profile");
                    // Using the logic from passport-github: the email scopes unlock the emails endpoint.
                    if (config.scopes.some((scope) => scope === "user" || scope === "user:email")) {
                        providerProfile.emails = await fetchProfile("https://api.github.com/user/emails", accessToken);
                    }
                    return { providerProfile, providerUserId: `${providerProfile.id}` };
                },
            };
        }
        case "slack": {
            const client = new Slack(requireEnv(runtime, "SLACK_CLIENT_ID"), requireEnv(runtime, "SLACK_CLIENT_SECRET"), callbackUrl);
            return {
                id,
                displayName: "Slack",
                oAuthType: "OAuth2",
                getAuthorizationUrl: ({ state }, config) => client.createAuthorizationURL(state, config),
                getProviderTokens: ({ code }) => client.validateAuthorizationCode(code),
                getProviderInfo: async ({ accessToken }) => {
                    const providerProfile = await fetchProfile("https://slack.com/api/openid.connect.userInfo", accessToken);
                    if (!providerProfile.sub)
                        throw new Error("Invalid profile");
                    return {
                        providerProfile,
                        providerUserId: providerProfile.sub,
                    };
                },
            };
        }
        case "discord": {
            const client = new Discord(requireEnv(runtime, "DISCORD_CLIENT_ID"), requireEnv(runtime, "DISCORD_CLIENT_SECRET"), callbackUrl);
            return {
                id,
                displayName: "Discord",
                oAuthType: "OAuth2",
                getAuthorizationUrl: ({ state }, config) => client.createAuthorizationURL(state, config),
                getProviderTokens: ({ code }) => client.validateAuthorizationCode(code),
                getProviderInfo: async ({ accessToken }) => {
                    const providerProfile = await fetchProfile("https://discord.com/api/users/@me", accessToken);
                    if (!providerProfile.id)
                        throw new Error("Invalid profile");
                    if (providerProfile.avatar) {
                        providerProfile.avatar = `https://cdn.discordapp.com/avatars/${providerProfile.id}/${providerProfile.avatar}.png`;
                    }
                    return {
                        providerProfile,
                        providerUserId: providerProfile.id,
                    };
                },
            };
        }
    }
}
