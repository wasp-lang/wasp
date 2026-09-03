import { findAuthWithUser } from "../email/flows.js";
import { type Route } from "../http.js";
import type { Ctx, OAuthProviderName } from "../types.js";
export declare const OAUTH_PROVIDER_NAMES: OAuthProviderName[];
/**
 * The OAuth methods: `/auth/<provider>/login`, `/auth/<provider>/callback`
 * and the shared `/auth/exchange-code` redemption -- the in-tree handler,
 * state/cookie and one-time-code machinery, on the contract's facets.
 */
export declare function oauthRoutes(ctx: Ctx): Route[];
export { findAuthWithUser };
