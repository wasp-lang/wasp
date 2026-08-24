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
const CLIENT_RUNTIME_BINDINGS_MODULE = "{= clientRuntimeBindingsEntryPointPath =}"

export function validateEnv(): Plugin {
  let resolvedConfig: ResolvedConfig;

  return {
    name: PLUGIN_NAME,
    configResolved(config) {
      resolvedConfig = config;
    },
    // We validate just before any artifacts are built.
    async buildStart() {
      await validateClientEnv(resolvedConfig);
    },
  };
}

export async function validateClientEnv(resolvedConfig: ResolvedConfig): Promise<void> {
  // We need to import the client env schema validation module through a Vite
  // server because both the user and Wasp schema modules may use bundler features.
  const tempServer = await createViteServer({
    root: resolvedConfig.root,
    mode: resolvedConfig.mode,
    // Reuse user-defined resolution and transforms while avoiding validation recursion.
    configFile: false,
    plugins: resolvedConfig.plugins
      .filter((plugin) => plugin.name !== PLUGIN_NAME)

      // Ignore `vite:`-prefixed plugins since Vite recreates them for the temporary server.
      .filter((plugin) => !plugin.name.startsWith("vite:"))

      // Avoid starting middleware, watchers, and other long-lived plugin behavior.
      .map((plugin) => ({
        ...plugin,
        configureServer: undefined,
        configurePreviewServer: undefined,
      })),

    // Minimize side effects from spinning up a temporary dev server.
    appType: 'custom',
    server: {
      middlewareMode: true,
      watch: null,
      hmr: false
    },
    logLevel: "silent",
    optimizeDeps: { noDiscovery: true, include: [] },
    clearScreen: false,
  });

  try {
    // Vite's `ssr` means bundled for a backend JS runtime such as Node.
    if (!isRunnableDevEnvironment(tempServer.environments.ssr)) {
      throw new Error(`Expected ssr to be a runnable dev environment`)
    }

    const runner = tempServer.environments.ssr.runner;
    await runner.import(CLIENT_RUNTIME_BINDINGS_MODULE);

    // Importing this module validates the client environment as a side effect.
    const moduleAbsPath = path.resolve(resolvedConfig.root, CLIENT_ENV_SCHEMA_VALIDATION_MODULE);
    await runner.import(moduleAbsPath);
  } finally {
    await tempServer.close();
  }
}
