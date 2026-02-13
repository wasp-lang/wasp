{{={= =}=}}
import { type PluginOption } from "vite";
import { defaultExclude } from "vitest/config"

// Vite merges `userConfig` and our `waspConfig` returned from the plugin.
// In that merge, primitive values from waspConfig take precedence, and
// arrays are concatenated.
//
// This allows us to treat config values differently:
//  - Forced: hardcoded in the return object so they always win. If the
//    user set one of these in their vite.config.ts, we log a warning.
//  - Overridable: we read the user's value from `config` and use it or
//    fall back to our default.
//  - Additive (arrays): we only return Wasp's entries; Vite's merge
//    appends them to whatever the user already has.

const forcedOptions = {
  'base': "{= baseDir =}",
  'envPrefix': "REACT_APP_",
  'server.port': {= defaultClientPort =},
  'build.outDir': "{= clientBuildDirPath =}",
} as const;

export function waspConfig(): PluginOption {
  return {
    name: "wasp:config",
    enforce: 'pre',
    config(config) {
      warnIfOverridingForcedOptions(config);

      return {
        base: forcedOptions['base'],
        optimizeDeps: {
          exclude: {=& depsExcludedFromOptimization =}
        },
        server: {
          port: forcedOptions['server.port'],
          host: overridable(config.server?.host, "0.0.0.0"),
          open: overridable(config.server?.open, true),
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
          globals: overridable((config as any).test?.globals, true),
          environment: overridable((config as any).test?.environment, "jsdom"),
          setupFiles: {=& vitest.setupFilesArray =},
          exclude: [
            ...defaultExclude,
            "{= vitest.excludeWaspArtefactsPattern =}",
          ]
        },
      };
    }
  };
}

function overridable<T>(userValue: T | undefined, defaultValue: T): T {
  return userValue ?? defaultValue;
}

function warnIfOverridingForcedOptions(config: Record<string, any>): void {
  for (const [path, forcedValue] of Object.entries(forcedOptions)) {
    const userValue = getByPath(config, path);
    const isOverriding = userValue !== undefined && userValue !== forcedValue;
    if (isOverriding) {
      console.warn(
        `[wasp:config] Ignoring Vite config "${path}" (set to ${JSON.stringify(userValue)}). ` +
        `Wasp requires the value to be ${JSON.stringify(forcedValue)}.`
      );
    }
  }
}

function getByPath(obj: Record<string, any>, path: string): unknown {
  return path.split('.').reduce<any>((node, segment) => node?.[segment], obj);
}
