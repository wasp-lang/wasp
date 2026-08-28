import { Router } from "express";

import username from './config/username.js'

const providers = [
  username,
];

const router = Router();


for (const provider of providers) {
  const { createRouter } = provider;
  const providerRouter = createRouter(provider);
  router.use(`/${provider.id}`, providerRouter);
  console.log(`🚀 "${provider.displayName}" auth initialized`);
}

export default router;
