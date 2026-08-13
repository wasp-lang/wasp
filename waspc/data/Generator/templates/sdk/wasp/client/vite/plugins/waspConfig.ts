{{={= =}=}}
/// <reference types="vitest/config" />
import { type PluginOption } from "vite";
import { defaultExclude } from "vitest/config";

// Vite merges `userConfig` and our `waspConfig` returned from the plugin.
// In that merge, primitive values from waspConfig take precedence, and
// arrays are concatenated.
//
// This allows us to treat config values differently:
//  - Forced: taken from `forcedOptions` in the return object so they
//    always win. If the user set one of these in their vite.config.ts,
//    we throw an error.
//  - Overridable: we read the user's value and use it or fall back to
//    our default.
//  - Additive (arrays): we only return Wasp's entries; Vite's merge
//    appends them to whatever the user already has.

const forcedOptions = {
  base: "{= baseDir =}",
  envPrefix: "REACT_APP_",
} as const;

const forcedOptionHints: Partial<Record<keyof typeof forcedOptions, string>> = {
  base: "To serve your app from a subdirectory, set `client.baseDir` in your Wasp config.",
};

export function waspConfig(): PluginOption {
  return {
    name: "wasp:config",
    enforce: "pre",
    config(config, env) {
      throwIfOverridingForcedOptions(config);

      const devServerPort = useUserValue(config.server?.port, {= defaultClientPort =});
      if (env.command === "serve" && !env.isPreview) {
        pinDevServerPort(devServerPort);
      }

      // Returned config is merged with the user's config by Vite (mergeConfig).
      return {
        base: forcedOptions["base"],
        optimizeDeps: {
          exclude: {=& depsExcludedFromOptimization =}
        },
        server: {
          port: devServerPort,
          host: useUserValue(config.server?.host, "0.0.0.0"),
        },
        envPrefix: forcedOptions["envPrefix"],
        // We don't set `build.outDir`: Nitro owns the build output and forces
        // the client's `outDir` to its own public directory. See the
        // `wasp:nitro-bridge` plugin.
        resolve: {
          // These packages rely on a single instance per page. Not deduping them
          // causes runtime errors (e.g., hook rule violation in react, QueryClient
          // instance error in react-query, Invariant Error in react-router).
          dedupe: [
            "react",
            "react-dom",
            "@tanstack/react-query",
            "react-router",
          ],
          alias: [
            {
              // Vite doesn't look for `.prisma/client` imports in the `node_modules`
              // folder. We point it to the correct place here.
              // TODO: Check if we can remove when updating Prisma (#2504)
              find: /^\.prisma\/client\/(.+)$/,
              replacement: "node_modules/.prisma/client/$1.js",
            },
            {
              // Handle bare .prisma/client import
              find: /^\.prisma\/client$/,
              replacement: "node_modules/.prisma/client",
            },
          ],
        },
        test: {
          globals: useUserValue(config.test?.globals, true),
          environment: useUserValue(config.test?.environment, "jsdom"),
          setupFiles: {=& vitest.setupFilesArray =},
          exclude: [
            ...defaultExclude,
            "{= vitest.excludeWaspArtefactsPattern =}",
          ],
        },
      };
    },
  };
}

function useUserValue<T>(userValue: T | undefined, defaultValue: T): T {
  return userValue ?? defaultValue;
}

/**
 * Nitro's dev server picks its port with `process.env.PORT || server.port ||
 * 3000`, so `PORT` wins over the port we (or the user) configured. It also
 * loads the project's `.env` files into `process.env` before reading it, and
 * `PORT` is a variable Wasp users commonly set for the server. Left alone, a
 * `PORT=3001` in a `.env` file would silently move the client's dev server.
 *
 * So we write the port we settled on into `process.env` ourselves. We set it
 * instead of deleting it because Nitro's `.env` loader never overwrites a
 * variable that is already defined.
 */
function pinDevServerPort(port: number): void {
  process.env.PORT = String(port);
}

function throwIfOverridingForcedOptions(config: Record<string, any>): void {
  const conflicts: string[] = [];
  for (const [path, forcedValue] of Object.entries(forcedOptions)) {
    const userValue = getByPath(config, path);
    if (userValue !== undefined && userValue !== forcedValue) {
      const hint = forcedOptionHints[path as keyof typeof forcedOptions];
      conflicts.push(
        `  - "${path}" is set to ${JSON.stringify(userValue)}, but Wasp requires ${JSON.stringify(forcedValue)}` +
          (hint ? `\n    ${hint}` : ""),
      );
    }
  }
  if (conflicts.length > 0) {
    throw new Error(
      `Your vite.config.ts sets options that Wasp controls:\n${conflicts.join("\n")}\n\nRemove these from your Vite config, Wasp sets them automatically.`,
    );
  }
}

function getByPath(obj: Record<string, any>, path: string): unknown {
  return path.split(".").reduce<any>((node, segment) => node?.[segment], obj);
}
