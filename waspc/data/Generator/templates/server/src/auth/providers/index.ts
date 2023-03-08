{{={= =}=}}

import { Router } from "express";

import { getDirFromFileUrl, importJsFilesFromDir } from "../../utils.js";

import { ProviderConfig } from "./types";

const allowedConfigs = [
  {=# enabledProviderIds =}
  "{= . =}.js",
  {=/ enabledProviderIds =}
];
const providers = await importProviders(allowedConfigs);

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

async function importProviders(providerConfigs: string[]): Promise<ProviderConfig[]> {
  const currentExecutionDir = getDirFromFileUrl(import.meta.url);
  const providers = await importJsFilesFromDir(currentExecutionDir, "./config", providerConfigs);
  return providers.map((provider) => provider.default);
}
