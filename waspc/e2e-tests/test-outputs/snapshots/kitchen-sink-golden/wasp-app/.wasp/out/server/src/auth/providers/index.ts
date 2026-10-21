import { createAuthProvidersRouter } from "@wasp.sh/lib-auth/node";
import { setupOneTimeCodeRoute } from "@wasp.sh/lib-auth/node";
import { exchangeCodeForTokenPath, tokenStore } from "wasp/server/auth";
import { sessions } from "wasp/server/auth/session";
import { authIdentities } from "wasp/server/auth/utils";

import slack from "./config/slack.js"
import discord from "./config/discord.js"
import github from "./config/github.js"
import google from "./config/google.js"
import microsoft from "./config/microsoft.js"
import email from "./config/email.js"

const providers = [
  slack,
  discord,
  github,
  google,
  microsoft,
  email,
];

const router = createAuthProvidersRouter(providers, (router) => {
  // Setting up a common route for all OAuth providers to exchange
  // one-time code for a session.
  setupOneTimeCodeRoute(router, {
    exchangeCodeForTokenPath,
    tokenStore,
    identities: authIdentities,
    sessions,
  });
});

export default router;
