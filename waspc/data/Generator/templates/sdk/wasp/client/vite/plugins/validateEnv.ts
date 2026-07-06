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
        plugins: resolvedConfig.plugins
          .filter((plugin) => plugin.name !== PLUGIN_NAME)
          // Reusing `vite:`-prefixed plugin instances is unsafe: the new
          // server re-runs their `configResolved`/`buildStart` hooks, and
          // since Vite 8 those hooks rebind closures shared with the main
          // server (e.g. `vite:resolve-builtin:get-environment` captures "the
          // current environment per name", and `vite:react-*` captures the
          // resolved config). The main server then resolves through this
          // temporary server's environments (deps optimizer with
          // `noDiscovery: true`, HMR disabled), breaking CJS dependency
          // pre-bundling and React Fast Refresh for the whole dev session.
          // The temporary server creates its own fresh internal plugins, and
          // Vite transforms TS/JSX natively, so dropping these is safe here.
          .filter((plugin) => !plugin.name.startsWith("vite:"))
          // Vite's `configureServer`/`configurePreviewServer` hooks let plugins
          // wire long-lived behavior into a dev or preview server: middleware,
          // websocket handlers, file watchers, and similar background tasks.
          //
          // Plugins are supposed to clean these up by returning a teardown
          // function from the hook, but some forget to, so resources they
          // allocate end up outliving the server. This forces the original
          // Vite process to be alive indefinitely.
          //
          // We don't need either hook to validate the client env schema.
          // We only need module resolution and transforms.
          //
          // `buildStart` must not run either: plugins may use it to capture
          // the environments of the server that fires it (Vite's own internal
          // plugins do this), and running it here would rebind such closures
          // to this short-lived server.
          .map((plugin) => ({
            ...plugin,
            configureServer: undefined,
            configurePreviewServer: undefined,
            buildStart: undefined,
          })),
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
