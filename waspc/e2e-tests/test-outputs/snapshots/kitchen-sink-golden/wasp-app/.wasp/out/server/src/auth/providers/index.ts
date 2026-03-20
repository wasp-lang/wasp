import { Router } from "express";
import { setupOneTimeCodeRoute } from "./oauth/oneTimeCode";

import google from './config/google.js'
import github from './config/github.js'
import discord from './config/discord.js'
import slack from './config/slack.js'
import microsoft from './config/microsoft.js'
import email from './config/email.js'

const providers = [
  google,
  github,
  discord,
  slack,
  microsoft,
  email,
];

const router = Router();

// Setting up a common route for all OAuth providers to exchange
// one-time code for a session.
setupOneTimeCodeRoute(router);

for (const provider of providers) {
  const { createRouter } = provider;
  const providerRouter = createRouter(provider);
  router.use(`/${provider.id}`, providerRouter);
  console.log(`🚀 "${provider.displayName}" auth initialized`);
}

export default router;
