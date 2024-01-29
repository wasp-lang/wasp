{{={= =}=}}

import { join } from 'path'
import { Router } from "express";

import { getDirPathFromFileUrl, importJsFilesFromDir } from "wasp/server/utils";

import { ProviderConfig } from "wasp/auth/providers/types";

const whitelistedProviderConfigFileNames = [
  {=# enabledProviderIds =}
  "{= . =}.js",
  {=/ enabledProviderIds =}
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
