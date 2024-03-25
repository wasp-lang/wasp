import { Router } from "express";
import { setupOneTimeCodeRoute } from "./oauth/oneTimeCode";

import __userDefinedGoogle from './config/google.js'

const providers = [
  __userDefinedGoogle,
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
