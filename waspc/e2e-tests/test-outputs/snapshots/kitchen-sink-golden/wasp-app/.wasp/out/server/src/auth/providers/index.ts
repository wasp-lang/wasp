import { Router } from "express";

import slack from './config/slack.js'
import discord from './config/discord.js'
import github from './config/github.js'
import google from './config/google.js'
import microsoft from './config/microsoft.js'
import email from './config/email.js'

const providers = [
  slack,
  discord,
  github,
  google,
  microsoft,
  email,
];

const router = Router();


for (const provider of providers) {
  const { createRouter } = provider;
  const providerRouter = createRouter(provider);
  router.use(`/${provider.id}`, providerRouter);
  console.log(`🚀 "${provider.displayName}" auth initialized`);
}

export default router;
