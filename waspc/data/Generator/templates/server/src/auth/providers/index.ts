{{={= =}=}}
import { Router } from "express";

{=# providers =}
{=& importStatement =}
{=/ providers =}

const providers = [
  {=# providers =}
  {= importIdentifier =},
  {=/ providers =}
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
