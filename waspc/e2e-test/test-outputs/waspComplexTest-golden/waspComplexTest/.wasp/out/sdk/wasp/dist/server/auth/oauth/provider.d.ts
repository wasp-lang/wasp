import { OAuth2Provider, OAuth2ProviderWithPKCE } from "arctic";
export declare function defineProvider<OAuthClient extends OAuth2Provider | OAuth2ProviderWithPKCE, Env extends Record<string, string>>({ id, displayName, env, oAuthClient, }: {
    id: string;
    displayName: string;
    env: Env;
    oAuthClient: OAuthClient;
}): {
    id: string;
    displayName: string;
    env: Env;
    oAuthClient: OAuthClient;
};
