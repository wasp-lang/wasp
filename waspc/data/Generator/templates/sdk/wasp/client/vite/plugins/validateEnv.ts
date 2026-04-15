import assert from "node:assert/strict";
import {
  type Plugin,
  type PluginOption,
  type ResolvedConfig,
  createServer as createViteServer,
  isRunnableDevEnvironment
} from "vite";

const CLIENT_ENV_SCHEMA_VALIDATION_MODULE = "wasp/client";

export function validateEnv(): PluginOption {
  return [validateEnvDev(), validateEnvBuild()];
}

function validateEnvDev(): Plugin {
  return {
    name: "wasp:validate-env:dev",
    apply: "serve",

    async configureServer(server) {
      assert(
        isRunnableDevEnvironment(server.environments.ssr),
        "Expected ssr to be a runnable dev environment",
      );
      await server.environments.ssr.runner.import(CLIENT_ENV_SCHEMA_VALIDATION_MODULE);
    },
  };
}

function validateEnvBuild(): Plugin {
  let resolvedConfig: ResolvedConfig;

  return {
    name: "wasp:validate-env:build",
    apply: "build",

    configResolved(config) {
      resolvedConfig = config; 
    },

    async buildStart() {
      const tempServer = await createViteServer({
        root: resolvedConfig.root,
        mode: resolvedConfig.mode,
        configFile: false,
        plugins: resolvedConfig.plugins.filter(
          (plugin) => plugin.name !== "wasp:validate-env:build", // would cause recursion
        ),
        server: { middlewareMode: true },
        logLevel: "silent",
        optimizeDeps: { noDiscovery: true, include: [] },
      });

      try {
        await tempServer.ssrLoadModule(CLIENT_ENV_SCHEMA_VALIDATION_MODULE);
      } finally {
        await tempServer.close();
      }
    },
  };
}
