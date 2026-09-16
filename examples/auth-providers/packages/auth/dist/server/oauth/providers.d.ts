import type { OAuthProviderName, WaspAuthRuntime } from "../types.js";
type ProviderInfo = {
    providerProfile: unknown;
    providerUserId: string;
};
/**
 * One OAuth provider's mechanics: the arctic client, whether PKCE is used,
 * and how to read the profile -- exactly what the in-tree per-provider
 * config files carried, keyed by provider id.
 */
export type OAuthProviderDefinition = {
    id: OAuthProviderName;
    displayName: string;
    oAuthType: "OAuth2" | "OAuth2WithPKCE";
    getAuthorizationUrl(state: {
        state: string;
        codeVerifier?: string;
    }, config: {
        scopes: string[];
    }): Promise<URL>;
    getProviderTokens(state: {
        code: string;
        codeVerifier?: string;
    }): Promise<{
        accessToken: string;
    }>;
    getProviderInfo(tokens: {
        accessToken: string;
    }, config: {
        scopes: string[];
    }): Promise<ProviderInfo>;
};
export declare function makeOAuthProvider(runtime: WaspAuthRuntime, id: OAuthProviderName, callbackUrl: string): OAuthProviderDefinition;
export {};
