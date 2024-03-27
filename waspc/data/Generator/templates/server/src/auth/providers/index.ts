{{={= =}=}}
import { Router } from "express";
{=# isExternalAuthEnabled =}
import { setupOneTimeCodeRoute } from "./oauth/oneTimeCode";
{=/ isExternalAuthEnabled =}

{=# providers =}
{=& importStatement =}
{=/ providers =}

const providers = [
  {=# providers =}
  {= importIdentifier =},
  {=/ providers =}
];

const router = Router();

{=# isExternalAuthEnabled =}
// Setting up a common route for all OAuth providers to exchange
// one-time code for a session.
setupOneTimeCodeRoute(router);
{=/ isExternalAuthEnabled =}

for (const provider of providers) {
  const { createRouter } = provider;
  const providerRouter = createRouter(provider);
  router.use(`/${provider.id}`, providerRouter);
  console.log(`🚀 "${provider.displayName}" auth initialized`);
}

export default router;
