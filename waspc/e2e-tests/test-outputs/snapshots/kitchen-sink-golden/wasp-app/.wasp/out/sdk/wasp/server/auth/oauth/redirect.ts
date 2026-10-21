import { createOAuthRedirects } from "@wasp.sh/lib-auth/node";
import { config } from "../../index.js";

// PRIVATE API (server)
export const oauthRedirects = createOAuthRedirects({
  frontendUrl: config.frontendUrl,
  serverUrl: config.serverUrl,
  loginPath: "login",
  callbackPath: "callback",
  exchangeCodeForTokenPath: "exchange-code",
  clientOAuthCallbackPath: "/oauth/callback",
});

// PRIVATE API (server)
export const {
  loginPath,
  exchangeCodeForTokenPath,
  callbackPath,
  getRedirectUriForOneTimeCode,
  handleOAuthErrorAndGetRedirectUri,
} = oauthRedirects;

// PRIVATE API (SDK)
export const { getRedirectUriForCallback } = oauthRedirects;
