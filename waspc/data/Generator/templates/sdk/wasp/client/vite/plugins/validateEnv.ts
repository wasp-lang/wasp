import {
  type Plugin,
  type ResolvedConfig,
  type ViteDevServer,
  createServer as createViteServer,
} from "vite";
import * as z from "zod";

import {
  formatZodEnvError,
  getValidatedEnvOrError,
} from "../../../env/validation.js";
import { colorize } from "../../../universal/ansiColors.js";
import { loadEnvVars } from "./envFile.js";

const VALIDATE_ENV_PLUGIN_NAME = "wasp:validate-env";
const CLIENT_ENV_SCHEMA_MODULE = "wasp/client/env/schema";

// When loading the client env schema for validation, we spin up a temporary
// Vite server so that bundler features (e.g., virtual modules) work.
// That inner server reuses the user's full plugin pipeline, which
// includes this plugin too. To stop recursion, we manually filter it out,
// plus any plugins that are unnecessary for schema loading.
const PLUGINS_TO_SKIP_IN_TEMP_VITE_SERVER = new Set([
  "wasp:validate-env",    // would cause recursion
  "wasp:typescript-check" // unnecessary
]);

export function validateEnv(): Plugin {
  let resolvedConfig: ResolvedConfig;

  return {
    name: "wasp:validate-env",

    configResolved(config) {
      resolvedConfig = config;
    },

    async buildStart() {
      if (resolvedConfig.command !== "build") {
        return;
      }

      const envVars = await loadEnvVars({
        rootDir: resolvedConfig.root,
        envPrefix: resolvedConfig.envPrefix!,
        loadDotEnvFile: false, // We expect users to provide env vars inline.
      });

      const tempServer = await createViteServer({
        root: resolvedConfig.root,
        mode: resolvedConfig.mode,
        configFile: false,
        plugins: resolvedConfig.plugins.filter(
          (plugin) => !PLUGINS_TO_SKIP_IN_TEMP_VITE_SERVER.has(plugin.name),
        ),
        server: { middlewareMode: true },
        logLevel: "silent",
        optimizeDeps: { noDiscovery: true, include: [] },
      });

      try {
        const validationErrorMessage = await validateClientEnvSchema(tempServer, envVars);
        if (validationErrorMessage) {
          this.error(colorize("red", validationErrorMessage));
        }
      } finally {
        await tempServer.close();
      }
    },

    async configureServer(server) {
      const envVars = await loadEnvVars({
        rootDir: resolvedConfig.root,
        envPrefix: resolvedConfig.envPrefix!,
        loadDotEnvFile: true,
      });

      const validationErrorMessage = await validateClientEnvSchema(server, envVars);
      if (!validationErrorMessage) {
        return;
      }

      const sendError = () => {
        server.ws.send({
          type: "error",
          err: { message: validationErrorMessage, stack: "" },
        });
      };

      server.ws.on("connection", sendError);
      server.httpServer?.once("close", () => {
        server.ws.off("connection", sendError);
      });
    },
  };
}

async function validateClientEnvSchema(
  server: ViteDevServer,
  envVars: {[key: string]: string }
): Promise<string | null> {
  const clientEnvSchema = await loadClientEnvSchema(server);
  const validationResult = getValidatedEnvOrError(envVars, clientEnvSchema);
  return validationResult.success ? null : formatZodEnvError(validationResult.error);
}

async function loadClientEnvSchema(server: ViteDevServer): Promise<z.ZodType> {
  const clientEnvSchemaModule = await server.ssrLoadModule(CLIENT_ENV_SCHEMA_MODULE);
  return clientEnvSchemaModule.clientEnvSchema;
}
