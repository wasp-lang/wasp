import { Router } from "express";

import google from './config/google.js'

const providers = [
  google,
];

const router = Router();

for (const provider of providers) {
  const { init, createRouter } = provider;
  const initData = init
    ? await init(provider)
    : undefined;
  const providerRouter = createRouter(provider, initData);
  router.use(`/${provider.id}`, providerRouter);
  console.log(`🚀 "${provider.displayName}" auth initialized`)
}

export default router;
