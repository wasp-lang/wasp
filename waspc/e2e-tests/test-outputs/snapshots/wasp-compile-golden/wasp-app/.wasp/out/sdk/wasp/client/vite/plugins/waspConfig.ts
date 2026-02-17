/// <reference types="vitest/config" />
import { type PluginOption } from "vite";
import { defaultExclude } from "vitest/config"

// Vite merges `userConfig` and our `waspConfig` returned from the plugin.
// In that merge, primitive values from waspConfig take precedence, and
// arrays are concatenated.
//
// This allows us to treat config values differently:
//  - Forced: hardcoded in the return object so they always win. If the
//    user set one of these in their vite.config.ts, we throw an error.
//  - Overridable: we read the user's value and use it or fall back to
//    our default.
//  - Additive (arrays): we only return Wasp's entries; Vite's merge
//    appends them to whatever the user already has.

const forcedOptions = {
  'base': "/",
  'envPrefix': "REACT_APP_",
  'build.outDir': ".wasp/out/web-app/build/",
} as const;

export function waspConfig(): PluginOption {
  return {
    name: "wasp:config",
    enforce: 'pre',
    config(config) {
      throwIfOverridingForcedOptions(config);

      // Returned config is merged with the user's config by Vite (mergeConfig).
      return {
        base: forcedOptions['base'],
        optimizeDeps: {
          exclude: ['wasp', '@wasp.sh/lib-auth']
        },
        server: {
          port: useUserValue(config.server?.port, 3000),
          host: useUserValue(config.server?.host, "0.0.0.0"),
          open: useUserValue(config.server?.open, true),
        },
        envPrefix: forcedOptions['envPrefix'],
        build: {
          outDir: forcedOptions['build.outDir'],
        },
        resolve: {
          // These packages rely on a single instance per page. Not deduping them
          // causes runtime errors (e.g., hook rule violation in react, QueryClient
          // instance error in react-query, Invariant Error in react-router).
          dedupe: ["react", "react-dom", "@tanstack/react-query", "react-router"],
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
          setupFiles: ['wasp/client/test/setup'],
          exclude: [
            ...defaultExclude,
            ".wasp/**/*",
          ]
        },
      };
    }
  };
}

function useUserValue<T>(userValue: T | undefined, defaultValue: T): T {
  return userValue ?? defaultValue;
}

function throwIfOverridingForcedOptions(config: Record<string, any>): void {
  const conflicts: string[] = [];
  for (const [path, forcedValue] of Object.entries(forcedOptions)) {
    const userValue = getByPath(config, path);
    if (userValue !== undefined && userValue !== forcedValue) {
      conflicts.push(
        `  - "${path}" is set to ${JSON.stringify(userValue)}, but Wasp requires ${JSON.stringify(forcedValue)}`
      );
    }
  }
  if (conflicts.length > 0) {
    throw new Error(
      `Your vite.config.ts sets options that Wasp controls:\n${conflicts.join('\n')}\n\nRemove these from your Vite config, Wasp sets them automatically.`
    );
  }
}

function getByPath(obj: Record<string, any>, path: string): unknown {
  return path.split('.').reduce<any>((node, segment) => node?.[segment], obj);
}
