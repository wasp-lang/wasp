import { Router } from "express";

import { getDirFromFileUrl, importJsFilesFromDir } from "../../utils.js";

import { ProviderConfig } from "./types";

const providers = await importProviders();

const router = Router();

for (const provider of providers) {
  const { init, setupRouter } = provider;
  let initData;
  if (init) {
    initData = await init(provider);
  }
  const providerRouter = setupRouter(provider, initData);
  router.use(`/${provider.slug}`, providerRouter);
  console.log(`🚀 "${provider.name}" auth initialized`)
}

export default router;

async function importProviders(): Promise<ProviderConfig[]> {
  const currentExecutionDir = getDirFromFileUrl(import.meta.url);
  const providers = await importJsFilesFromDir(currentExecutionDir, "./config");
  return providers.map((provider) => provider.default);
}
