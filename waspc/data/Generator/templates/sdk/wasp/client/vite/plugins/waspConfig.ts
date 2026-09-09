{{={= =}=}}
/// <reference types="vitest/config" />
import type { ConfigEnv, PluginOption } from "vite";
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

export function waspConfig(): PluginOption {
  return {
    name: "wasp:config",
    enforce: "pre",
{=# isSingleDeploymentAndDevelopment =}
    configureServer(server) {
      if (isAppDevServer(server.config)) {
        throwIfDevProxyTargetMissing();
      }
    },
{=/ isSingleDeploymentAndDevelopment =}
    config(
      config,
{=# isSingleDeploymentAndDevelopment =}
      configEnv,
{=/ isSingleDeploymentAndDevelopment =}
    ) {
      throwIfOverridingForcedOptions(config);

      // Returned config is merged with the user's config by Vite (mergeConfig).
      return {
        base: forcedOptions["base"],
        optimizeDeps: {
          exclude: {=& depsExcludedFromOptimization =}
        },
        server: {
          port: forcedOptions["server.port"],
          strictPort: forcedOptions["server.strictPort"],
          host: useUserValue(config.server?.host, "0.0.0.0"),
{=# isSingleDeploymentAndDevelopment =}
          proxy: makeDevServerProxy(configEnv),
{=/ isSingleDeploymentAndDevelopment =}
        },
        preview: {
          port: forcedOptions["preview.port"],
        },
        envPrefix: forcedOptions["envPrefix"],
        build: {
          outDir: forcedOptions["build.outDir"],
        },
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

const forcedOptions = {
  base: "{= baseDir =}",
  envPrefix: "REACT_APP_",
  "build.outDir": "{= clientBuildDirPath =}",
  // Heads up! The env referred to by `clientPortEnvVarName` is empty during
  // `build`, so it's not persisted in the final output.
  "server.port": envVarAsNumber("{= clientPortEnvVarName =}"),
  "server.strictPort": true,
  // `vite preview` falls back to `server` for most options, but not for `port`
  // (it has its own default), so we have to set it separately.
  "preview.port": envVarAsNumber("{= clientPortEnvVarName =}"),
} as const;

const forcedOptionHints: Partial<Record<keyof typeof forcedOptions, string>> = {
  base: "To serve your app from a subdirectory, set `client.baseDir` in your Wasp config.",
  "server.port":
    "To run the client on a different port, use `wasp start --client-port <port>`.",
  "preview.port":
    "To run the client on a different port, use `wasp build start --client-port <port>`.",
};

function useUserValue<T>(userValue: T | undefined, defaultValue: T): T {
  return userValue ?? defaultValue;
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

function envVarAsNumber(envName: string): number | undefined {
  const strValue = process.env[envName];
  if (strValue === undefined) {
    return undefined;
  }
  const numValue = Number.parseInt(strValue);
  if (Number.isNaN(numValue)) {
    throw new Error(`Environment variable ${envName} is not a valid number.`);
  }
  return numValue;
}
{=# isSingleDeploymentAndDevelopment =}

// The client and the server share one origin. In development, the Vite dev
// server forwards Wasp's server routes (and the user's `api` routes) to the
// server process, whose URL `wasp start` passes in the env var below.
//
// `server.proxy` is not a forced option: entries from the user's
// `vite.config.ts` are merged with these, so users can proxy extra paths to
// the server by targeting `process.env.{= devProxyTargetEnvVarName =}`.
const devProxyTargetEnvVarName = "{= devProxyTargetEnvVarName =}";

const proxiedPathPrefixes: string[] = {=& proxiedPathPrefixes =};

function makeDevServerProxy(
  configEnv: ConfigEnv,
): Record<string, { target: string; changeOrigin: boolean; ws: boolean }> | undefined {
  if (!isAppDevServer(configEnv)) {
    return undefined;
  }
  // Other Wasp plugins (e.g. `wasp:validate-env`) create throwaway
  // middleware-mode servers during `vite build`, which also report `serve`
  // and never have the env var. `throwIfDevProxyTargetMissing` reports it from
  // `configureServer`, which only real dev servers run.
  const target = process.env[devProxyTargetEnvVarName];
  if (target === undefined) {
    return undefined;
  }
  return Object.fromEntries(
    proxiedPathPrefixes.map((pathPrefix) => [
      makePathPrefixProxyKey(pathPrefix),
      { target, changeOrigin: false, ws: true },
    ]),
  );
}

// Vite treats plain string keys as `startsWith` matches, which would also
// swallow unrelated client routes (e.g. "/api" would match an "/apis" page),
// so the key is an anchored regex that only matches whole path segments.
function makePathPrefixProxyKey(pathPrefix: string): string {
  return `^${escapeRegExp(pathPrefix)}(?:/|\\?|$)`;
}

function escapeRegExp(text: string): string {
  return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function throwIfDevProxyTargetMissing(): void {
  if (process.env[devProxyTargetEnvVarName] === undefined) {
    throw new Error(
      `The environment variable ${devProxyTargetEnvVarName} is not set. It tells the ` +
        "client dev server where the Wasp server is, so it can forward Wasp's routes to it.\n" +
        "Run your app with `wasp start`, which sets it for you. If you are starting the " +
        `dev server yourself, set it to the Wasp server's URL first, for example ` +
        `${devProxyTargetEnvVarName}=http://localhost:3001.`,
    );
  }
}

// The dev server that serves your app, as opposed to the other things Vite
// reports as `serve`: `vite preview` serves the built client on its own, and
// Vitest starts a dev server that has nothing to proxy.
function isAppDevServer({
  command,
  mode,
  isPreview,
}: Pick<ConfigEnv, "command" | "mode" | "isPreview">): boolean {
  return command === "serve" && !isPreview && mode !== "test";
}
{=/ isSingleDeploymentAndDevelopment =}
