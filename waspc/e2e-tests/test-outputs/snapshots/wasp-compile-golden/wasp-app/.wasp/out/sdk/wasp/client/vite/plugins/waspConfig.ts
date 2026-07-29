/// <reference types="vitest/config" />
import { type PluginOption } from "vite";
import { defaultExclude } from "vitest/config"

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
  'base': "/",
  'envPrefix': "REACT_APP_",
  'build.outDir': ".wasp/out/web-app/build/",
  // Wasp owns the dev server port: it can auto-pick a free one when the default
  // is taken, and it tells the server about the client's URL. Letting the port
  // be set here too would make those two disagree.
  'server.port': getClientDevPort(),
  // Without this, Vite silently moves to the next free port when the one it was
  // given is taken, which would make the client run on a URL the server doesn't
  // know about. We'd rather fail loudly.
  'server.strictPort': true,
} as const;

const forcedOptionHints: Partial<Record<keyof typeof forcedOptions, string>> = {
  'server.port': "To run the client on a different port, use `wasp start --client-port <port>`.",
  'server.strictPort': "Wasp needs the client to fail on a taken port instead of silently moving to another one.",
};

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
          exclude: ['wasp', '@wasp.sh/lib-auth', '@wasp.sh/lib-vite-ssr']
        },
        server: {
          port: forcedOptions['server.port'],
          strictPort: forcedOptions['server.strictPort'],
          host: useUserValue(config.server?.host, "0.0.0.0"),
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

// Wasp passes the dev server port through an env var instead of `vite --port`
// because CLI args end up in the same user config object we inspect above, which
// would make a Wasp-set port indistinguishable from a user-set one.
//
// It is unset whenever Vite runs outside of `wasp start` (`vite build`, Vitest,
// ...), where there is no dev server that needs a port.
function getClientDevPort(): number | undefined {
  const rawPort = process.env["PORT"];
  if (rawPort === undefined) {
    return undefined;
  }
  const port = Number(rawPort);
  if (!Number.isInteger(port) || port < 1 || port > 65535) {
    throw new Error(
      `Wasp set PORT to ${JSON.stringify(rawPort)}, which is not a valid port.`
    );
  }
  return port;
}

function throwIfOverridingForcedOptions(config: Record<string, any>): void {
  const conflicts: string[] = [];
  for (const [path, forcedValue] of Object.entries(forcedOptions)) {
    const userValue = getByPath(config, path);
    if (userValue !== undefined && userValue !== forcedValue) {
      const hint = forcedOptionHints[path as keyof typeof forcedOptions];
      // Wasp only sets some of these while it is starting your app, so there isn't
      // always a value we can point at as the one it requires.
      const requirement =
        forcedValue === undefined
          ? "but Wasp sets it itself"
          : `but Wasp requires ${JSON.stringify(forcedValue)}`;
      conflicts.push(
        `  - "${path}" is set to ${JSON.stringify(userValue)}, ${requirement}` +
          (hint ? `\n    ${hint}` : "")
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
