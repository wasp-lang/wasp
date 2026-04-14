import {
  type Plugin,
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

// When loading the client env schema for validation, 
// we spin up a temporary Vite server so that bundler features work. 
// This flag prevents the temporary server from recursively running its own validation.
const SKIP_FLAG = "__WASP_SKIP_CLIENT_ENV_VALIDATION__";

export function validateEnv(): Plugin {
  let envVars: {[key: string]: string };
  
  return {
    name: "wasp:validate-env",
    async configResolved(config) {
      if (process.env[SKIP_FLAG]) {
        return;
      }

      envVars = await loadEnvVars({
        rootDir: config.root,
        envPrefix: config.envPrefix!,
        loadDotEnvFile: config.command === "serve",
      });

      if (config.command === "build") {
        await withModuleLoaderGuard(async () => {
          const server = await createModuleLoaderViteDevServer(config.root, config.mode);
          try {
            await validateClientEnvSchema(server, envVars, (validationErrorMessage) => {
              console.error(colorize("red", validationErrorMessage));
              process.exit(1);
            })
          } finally {
            await server.close();
          }
        })
      }
    },
    configureServer(server) {
      if (process.env[SKIP_FLAG]) {
        return;
      }

      validateClientEnvSchema(server, envVars, (validationErrorMessage) => {
        const sendError = () => {
          server.ws.send({
            type: "error",
            err: { message: validationErrorMessage, stack: "" },
          });
        };
        server.ws.on("connection", sendError);
        sendError();
      });
    },
  };
}

async function withModuleLoaderGuard<T>(fn: () => Promise<T>): Promise<T> {
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
  envVars: {[key: string]: string }, 
  onValidationFailure: (validationErrorMessage: string) => void) {
  const clientEnvSchema = await loadClientEnvSchema(server);
  const validationResult = getValidatedEnvOrError(envVars, clientEnvSchema);
  if (!validationResult.success) {
    const validationErrorMessage = formatZodEnvError(validationResult.error);
    onValidationFailure(validationErrorMessage);
  }
}

async function loadClientEnvSchema(server: ViteDevServer): Promise<z.ZodType> {
  const clientEnvSchemaModule = await server.ssrLoadModule("wasp/client/env/schema");
  return clientEnvSchemaModule.clientEnvSchema;
}
