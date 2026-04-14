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
import { loadEnvVars } from "./envFile.js";

const CLIENT_ENV_SCHEMA_MODULE = "wasp/client/env/schema";

// When loading the client env schema for validation, we spin up a temporary
// Vite server so that bundler features (virtual modules, plugin resolution)
// work. That inner server reuses the user's full plugin pipeline, which
// includes this plugin, so we use a process-level flag to prevent the inner
// server from recursively running its own validation.
const SKIP_FLAG = "__WASP_SKIP_CLIENT_ENV_VALIDATION__";

export function validateEnv(): Plugin {
  let resolvedConfig: ResolvedConfig;
  
  return {
    name: "wasp:validate-env",

    configResolved(config) {
      resolvedConfig = config;
    },

    // Both dev and prod, but we only handle the prod branch here.
    async buildStart() {
      if (process.env[SKIP_FLAG] || resolvedConfig.command !== "build") {
        return;
      }

      const envVars = await loadEnvVars({
        rootDir: resolvedConfig.root,
        envPrefix: resolvedConfig.envPrefix!,
        loadDotEnvFile: resolvedConfig.command === "serve",
      });

      await runWithoutClientEnvValidation(async () => {
        const server = await createModuleLoaderViteDevServer(
          resolvedConfig.root,
          resolvedConfig.mode,
        );
        try {
          const validationErrorMessage = await validateClientEnvSchema(server, envVars);
          if (validationErrorMessage) {
            this.error(validationErrorMessage);
          }
        } finally {
          await server.close();
        }
      });
    },

    // Dev server only.
    async configureServer(server) {
      if (process.env[SKIP_FLAG]) {
        return;
      }

      const envVars = await loadEnvVars({
        rootDir: resolvedConfig.root,
        envPrefix: resolvedConfig.envPrefix!,
        loadDotEnvFile: resolvedConfig.command === "serve",
      });

      const validationErrorMessage = validateClientEnvSchema(server, envVars);
      const sendError = () => {
        server.ws.send({
          type: "error",
          err: { message: validationErrorMessage, stack: "" },
        });
      };
      server.ws.on("connection", sendError);
      sendError();
    },
  };
}

async function runWithoutClientEnvValidation<T>(fn: () => Promise<T>): Promise<T> {
  process.env[SKIP_FLAG] = "1";
  try {
    return await fn();
  } finally {
    delete process.env[SKIP_FLAG];
  }
}

async function createModuleLoaderViteDevServer(rootDir: string, mode: string): Promise<ViteDevServer> {
  return await createViteServer({
    root: rootDir,
    mode,
    server: { middlewareMode: true },
    logLevel: "silent",
    optimizeDeps: { noDiscovery: true, include: [] },
  });
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
