import { OAuth2Provider, OAuth2ProviderWithPKCE } from "arctic";
import { type ProviderName } from "../../../auth/utils.js";

export function defineProvider<
  OAuthClient extends OAuth2Provider | OAuth2ProviderWithPKCE,
  Env extends Record<string, string>
>({
  id,
  displayName,
  env,
  oAuthClient,
}: {
  id: ProviderName;
  displayName: string;
  env: Env;
  oAuthClient: OAuthClient;
}) {
  return {
    id,
    displayName,
    env,
    oAuthClient,
  };
}
