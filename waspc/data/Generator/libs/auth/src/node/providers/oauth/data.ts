import type {
  DiscordTokens,
  GitHubTokens,
  GoogleTokens,
  KeycloakTokens,
  MicrosoftEntraIdTokens,
  SlackTokens,
} from "arctic";
import type { OAuthFlowData } from "../../hooks.js";

export type OAuthTokensByProvider = {
  google: GoogleTokens;
  discord: DiscordTokens;
  github: GitHubTokens;
  keycloak: KeycloakTokens;
  microsoft: MicrosoftEntraIdTokens;
  slack: SlackTokens;
};

export type OAuthProviderName = keyof OAuthTokensByProvider;

/**
 * The `OAuthData` the hooks receive, for the given enabled OAuth providers:
 * a discriminated union of provider names and their tokens.
 */
export type OAuthDataFor<EnabledProvider extends OAuthProviderName> =
  OAuthFlowData &
    {
      [PN in EnabledProvider]: {
        providerName: PN;
        tokens: OAuthTokensByProvider[PN];
      };
    }[EnabledProvider];
