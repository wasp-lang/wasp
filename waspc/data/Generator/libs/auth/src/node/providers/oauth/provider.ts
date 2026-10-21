import type { OAuth2Provider, OAuth2ProviderWithPKCE } from "arctic";
import type { ProviderName } from "../../../shared/providerData.js";

/** An OAuth provider as `defineProvider` returns it: its id, display name and arctic client. */
export type DefinedOAuthProvider<
  OAuthClient extends OAuth2Provider | OAuth2ProviderWithPKCE,
  Id extends ProviderName = ProviderName,
> = {
  id: Id;
  displayName: string;
  oAuthClient: OAuthClient;
};

export function defineProvider<
  OAuthClient extends OAuth2Provider | OAuth2ProviderWithPKCE,
  const Id extends ProviderName,
>({
  id,
  displayName,
  oAuthClient,
}: {
  id: Id;
  displayName: string;
  oAuthClient: OAuthClient;
}): DefinedOAuthProvider<OAuthClient, Id> {
  return {
    id,
    displayName,
    oAuthClient,
  };
}
