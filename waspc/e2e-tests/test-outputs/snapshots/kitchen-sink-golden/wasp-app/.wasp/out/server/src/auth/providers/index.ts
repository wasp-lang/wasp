import { Router } from "express";
import { setupOneTimeCodeRoute } from "./oauth/oneTimeCode";

import slack from './config/slack.js'
import discord from './config/discord.js'
import github from './config/github.js'
import google from './config/google.js'
import email from './config/email.js'

const providers = [
  slack,
  discord,
  github,
  google,
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
