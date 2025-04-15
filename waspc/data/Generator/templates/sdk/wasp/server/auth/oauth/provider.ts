import { OAuth2Provider, OAuth2ProviderWithPKCE } from "arctic";

export function defineProvider<
  OAuthClient extends OAuth2Provider | OAuth2ProviderWithPKCE,
  const Id extends string
>({
  id,
  displayName,
  oAuthClient,
}: {
  id: Id;
  displayName: string;
  oAuthClient: OAuthClient;
}) {
  return {
    id,
    displayName,
    oAuthClient,
  };
}
