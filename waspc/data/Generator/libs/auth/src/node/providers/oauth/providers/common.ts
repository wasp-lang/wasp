import type { OAuth2Provider, OAuth2ProviderWithPKCE } from "arctic";
import type { UserSignupFields } from "../../../identities.js";
import type { UserDefinedConfigFn } from "../config.js";
import type { DefinedOAuthProvider } from "../provider.js";

/** What the app decides about one of its OAuth providers. */
export type OAuthProviderAuthConfig<
  OAuthClient extends OAuth2Provider | OAuth2ProviderWithPKCE,
> = {
  provider: DefinedOAuthProvider<OAuthClient>;
  /** Scopes Wasp needs to read the user's profile; the developer's config adds to them. */
  requiredScopes: string[];
  userDefinedConfigFn: UserDefinedConfigFn | undefined;
  userSignupFields: UserSignupFields | undefined;
};

export type ProviderProfile = {
  providerProfile: unknown;
  providerUserId: string;
};
