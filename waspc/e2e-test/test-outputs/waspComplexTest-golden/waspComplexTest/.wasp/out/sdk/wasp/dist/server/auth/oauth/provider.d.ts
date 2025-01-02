import { OAuth2Provider, OAuth2ProviderWithPKCE } from "arctic";
export declare function defineProvider<OAuthClient extends OAuth2Provider | OAuth2ProviderWithPKCE>({ id, displayName, oAuthClient, }: {
    id: string;
    displayName: string;
    oAuthClient: OAuthClient;
}): {
    id: string;
    displayName: string;
    oAuthClient: OAuthClient;
};
