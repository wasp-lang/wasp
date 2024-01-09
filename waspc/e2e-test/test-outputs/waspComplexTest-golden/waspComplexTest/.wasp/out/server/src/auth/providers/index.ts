
import { join } from 'path'
import { Router } from "express";

import { getDirPathFromFileUrl, importJsFilesFromDir } from "../../utils.js";

import { ProviderConfig } from "./types";

const whitelistedProviderConfigFileNames = [
  "google.js",
];
const providers = await importProviders(whitelistedProviderConfigFileNames);

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

async function importProviders(whitelistedProviderConfigFileNames: string[]): Promise<ProviderConfig[]> {
  const currentExecutionDir = getDirPathFromFileUrl(import.meta.url);
  const pathToDirWithConfigs = join(currentExecutionDir, "./config");
  const providers = await importJsFilesFromDir(pathToDirWithConfigs, whitelistedProviderConfigFileNames);
  return providers.map((provider) => provider.default);
}
