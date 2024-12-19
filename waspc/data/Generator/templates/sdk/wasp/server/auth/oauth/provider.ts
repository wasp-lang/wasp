import { OAuth2Provider, OAuth2ProviderWithPKCE } from "arctic";

export function defineProvider<
  OAuthClient extends OAuth2Provider | OAuth2ProviderWithPKCE
>({
  id,
  displayName,
  oAuthClient,
}: {
  id: string;
  displayName: string;
  oAuthClient: OAuthClient;
}) {
  return {
    id,
    displayName,
    oAuthClient,
  };
}
