import path from "node:path";
import {
  type Plugin,
  type ResolvedConfig,
  createServer as createViteServer,
  isRunnableDevEnvironment
} from "vite";

const PLUGIN_NAME = "wasp:validate-env";
const CLIENT_ENV_SCHEMA_VALIDATION_MODULE = ".wasp/out/sdk/wasp/client/env.ts"

export function validateEnv(): Plugin {
  let resolvedConfig: ResolvedConfig;

  return {
    name: PLUGIN_NAME,
    configResolved(config) {
      resolvedConfig = config; 
    },
    async buildStart() {
      const tempServer = await createViteServer({
        root: resolvedConfig.root,
        mode: resolvedConfig.mode,
        // To ensure we pick up all user-defined plugins, while avoiding recursion.
        // This includes the `wasp` plugin which resolves virtual modules.
        configFile: false,
        plugins: resolvedConfig.plugins.filter(
          (plugin) => plugin.name !== PLUGIN_NAME
        ),
        // Minimize the possible side-effects.
        appType: 'custom',
        server: { middlewareMode: true, watch: null, hmr: false },
        logLevel: "silent",
        optimizeDeps: { noDiscovery: true, include: [] },
        clearScreen: false,
      });

      try {
        if (!isRunnableDevEnvironment(tempServer.environments.ssr)) {
          throw new Error(`Expected ssr to be a runnable dev environment`)
        }
        const moduleAbsPath = path.resolve(resolvedConfig.root, CLIENT_ENV_SCHEMA_VALIDATION_MODULE);
        await tempServer.environments.ssr.runner.import(moduleAbsPath);
      } finally {
        await tempServer.close();
      }
    },
  };
}
