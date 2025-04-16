import { OAuth2Provider, OAuth2ProviderWithPKCE } from "arctic";
export declare function defineProvider<OAuthClient extends OAuth2Provider | OAuth2ProviderWithPKCE, const Id extends string>({ id, displayName, oAuthClient, }: {
    id: Id;
    displayName: string;
    oAuthClient: OAuthClient;
}): {
    id: Id;
    displayName: string;
    oAuthClient: OAuthClient;
};
