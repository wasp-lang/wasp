{{={= =}=}}
import react from "@vitejs/plugin-react";
import type { Options as ReactOptions } from "@vitejs/plugin-react";
import path from "node:path";
import fs from "node:fs";
import { type Plugin, mergeConfig } from "vite";
import { detectServerImports } from "./detectServerImports.js";
import { validateEnv } from "./validateEnv.js";
import { virtualFiles } from "./virtualFiles.js";
import { parse as dotenvParse } from "dotenv";
import { expand as dotenvExpand } from "dotenv-expand";

export interface WaspPluginOptions {
  reactOptions?: ReactOptions;
}

/**
 * Load environment variables from .env.client file only.
 * This prevents server environment variables from leaking into the client bundle.
 *
 * @param envDir - Directory to look for .env.client file
 * @param envPrefix - Only expose variables with this prefix (e.g., 'REACT_APP_')
 * @returns Object with env vars that match the prefix
 */
function loadClientEnv(
  envDir: string,
  envPrefix: string | string[]
): Record<string, string> {
  const envFilePath = path.join(envDir, ".env.client");
  const clientEnv: Record<string, string> = {};

  if (!fs.existsSync(envFilePath)) {
    return clientEnv;
  }

  // Parse .env.client using dotenv (same library Vite uses)
  const envFileContent = fs.readFileSync(envFilePath, "utf-8");
  const parsed = dotenvParse(envFileContent);

  // Expand variables (e.g., KEY=$OTHER_KEY) using dotenv-expand
  const expanded = dotenvExpand({
    parsed,
    // Don't mutate process.env
    ignoreProcessEnv: true,
  });

  if (!expanded.parsed) {
    return clientEnv;
  }

  // Filter to only include variables with the specified prefix
  const prefixes = Array.isArray(envPrefix) ? envPrefix : [envPrefix];
  for (const [key, value] of Object.entries(expanded.parsed)) {
    if (value !== undefined && prefixes.some((prefix) => key.startsWith(prefix))) {
      clientEnv[key] = value;
    }
  }

  return clientEnv;
}

export function wasp(options?: WaspPluginOptions): Plugin[] {
  return [
    virtualFiles(),
    validateEnv(),
    ...react(options?.reactOptions),
    detectServerImports(),
    {
      name: "wasp-config",
      config(config) {
        const envPrefix = "REACT_APP_";

        // Load ONLY .env.client to prevent server env vars from leaking into client bundle.
        const clientEnv = loadClientEnv(process.cwd(), envPrefix);

        // Inject client env vars into Vite's define
        const define: Record<string, string> = {};
        for (const [key, value] of Object.entries(clientEnv)) {
          define[`import.meta.env.${key}`] = JSON.stringify(value);
        }

        return mergeConfig({
          // Set root to project directory so Vite finds index.html and node_modules correctly
          root: process.cwd(),
          base: "{= baseDir =}",
          // Disable Vite's automatic .env file loading to prevent loading .env.server
          envDir: false,
          // Manually define the env vars we loaded from .env.client
          define,
          optimizeDeps: {
            exclude: ["wasp"],
          },
          server: {
            port: {= defaultClientPort =},
            host: "0.0.0.0",
            open: true,
          },
          envPrefix: "REACT_APP_",
          build: {
            outDir: "{= buildOutputDir =}",
          },
          resolve: {
            // These packages rely on a single instance per page. Not deduping them
            // causes runtime errors (e.g., hook rule violation in react, QueryClient
            // instance error in react-query, Invariant Error in react-router-dom).
            dedupe: [
              "react",
              "react-dom",
              "@tanstack/react-query",
              "react-router-dom",
            ],
            alias: [
              {
                // Vite doesn't look for `.prisma/client` imports in the `node_modules`
                // folder. We point it to the correct place here.
                // TODO: Check if we can remove when updating Prisma (#2504)
                find: /^\.prisma\/(.+)$/,
                replacement: path.join("{= projectDir =}", "node_modules/.prisma/$1"),
              },
            ],
          },
          test: (config.test as any)
            ? {
              // TODO: note sure if we need to do this if using mergeConfig
                exclude: [
                  ...((config.test as any).exclude || []),
                  "{= vitest.excludeWaspArtefactsPattern =}",
                ],
              }
            : undefined,
        }, config);
      },
    } as any,
  ];
}
