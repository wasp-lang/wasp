import type { OAuthDependencies } from "@wasp.sh/lib-auth/node";
import { config } from "wasp/server";
import {
  oauthRedirects,
  tokenStore,
  type AuthHookTypes,
} from "wasp/server/auth";
import { authIdentities } from "wasp/server/auth/utils";
import * as hooks from "../../hooks.js";

/** What the OAuth providers need from the framework and the app. */
export const oauthDependencies: OAuthDependencies<AuthHookTypes> = {
  identities: authIdentities,
  hooks,
  redirects: oauthRedirects,
  tokenStore,
  isDevelopment: config.isDevelopment,
};
