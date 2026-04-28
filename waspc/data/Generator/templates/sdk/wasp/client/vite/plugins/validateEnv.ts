{{={= =}=}}
import path from "node:path";
import {
  type Plugin,
  type ResolvedConfig,
  createServer as createViteServer,
  isRunnableDevEnvironment
} from "vite";

const PLUGIN_NAME = "wasp:validate-env";
const CLIENT_ENV_SCHEMA_VALIDATION_MODULE = "{= clientEnvSchemaValidationModulePath =}"

export function validateEnv(): Plugin {
  let resolvedConfig: ResolvedConfig;

  return {
    name: PLUGIN_NAME,
    configResolved(config) {
      resolvedConfig = config; 
    },
    // We validate just before any artifacts are built.
    async buildStart() {
      // We need to import the client env schema validation module
      // through a Vite server, because both the user and the Wasp schema
      // modules may depend on bundler features.
      // Because of that we spin up a tepomrary Vite server.
      //
      // Alternatively, for `serve`, we could use the Vite server provided
      // through the `configureServer` hook, but that would complicate
      // the solution for negligible performance benefits. 
      const tempServer = await createViteServer({
        root: resolvedConfig.root,
        mode: resolvedConfig.mode,
        // To ensure we pick up all user-defined plugins (resolution matches the main build)
        // while avoiding recursion. This includes the `wasp` plugin.
        configFile: false,
        plugins: resolvedConfig.plugins.filter(
          (plugin) => plugin.name !== PLUGIN_NAME
        ),
        // Minimize side effects from spinning up a temporary dev server.
        appType: 'custom',      // avoid HTML handling
        server: { 
          middlewareMode: true, // do not start an actual HTTP server
          watch: null, 
          hmr: false 
        },
        logLevel: "silent",
        optimizeDeps: { noDiscovery: true, include: [] },
        clearScreen: false,
      });

      try {
        // Vite's `ssr` means bundled for "backend JS runtime", like Node.
        // This envrionemnt is always runnable in vite dev server.
        if (!isRunnableDevEnvironment(tempServer.environments.ssr)) {
          throw new Error(`Expected ssr to be a runnable dev environment`)
        }
        // The imported module runs env schema validation as an import
        // side-effect and throws on failure. 
        const moduleAbsPath = path.resolve(resolvedConfig.root, CLIENT_ENV_SCHEMA_VALIDATION_MODULE);
        await tempServer.environments.ssr.runner.import(moduleAbsPath);
      } finally {
        await tempServer.close();
      }
    },
  };
}
