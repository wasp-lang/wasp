import { Router } from "express";

import email from './config/email.js'

const providers = [
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
