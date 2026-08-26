{{={= =}=}}
import { Router } from "express";
{=# usesSessionHandoff =}
import { setupSessionHandoffExchangeRoute } from "./oauth/sessionHandoff";
{=/ usesSessionHandoff =}

{=# providers =}
{=& importStatement =}
{=/ providers =}

const providers = [
  {=# providers =}
  {= importIdentifier =},
  {=/ providers =}
];

const router = Router();

{=# usesSessionHandoff =}
setupSessionHandoffExchangeRoute(router);
{=/ usesSessionHandoff =}

for (const provider of providers) {
  const { createRouter } = provider;
  const providerRouter = createRouter(provider);
  router.use(`/${provider.id}`, providerRouter);
  console.log(`🚀 "${provider.displayName}" auth initialized`);
}

export default router;
