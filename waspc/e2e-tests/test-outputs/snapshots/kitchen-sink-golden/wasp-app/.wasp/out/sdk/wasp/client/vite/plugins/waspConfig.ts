import type { PluginOption } from "vite";

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
  base: "/",
  envPrefix: "REACT_APP_",
  "build.outDir": ".wasp/out/web-app/build/",
  // Heads up! The env referred to by `clientPortEnvVarName` is empty during
  // `build`, so it's not persisted in the final output.
  "server.port": envVarAsNumber("PORT"),
  "server.strictPort": true,
  // `vite preview` falls back to `server` for most options, but not for `port`
  // (it has its own default), so we have to set it separately.
  "preview.port": envVarAsNumber("PORT"),
} as const;

const forcedOptionHints: Partial<Record<keyof typeof forcedOptions, string>> = {
  base: "To serve your app from a subdirectory, set `client.baseDir` in your Wasp config.",
  "server.port":
    "To run the client on a different port, use `wasp start --client-port <port>`.",
  "preview.port":
    "To run the client on a different port, use `wasp build start --client-port <port>`.",
};

// `wasp start` provides these env vars, but tests run without it. A test can't
// reach the server anyway (requests are usually mocked), so placeholder values
// are enough to pass the client env validation.
const testModeEnvVarDefaults = {
  "REACT_APP_API_URL": "http://localhost:3001",
};

export function waspConfig(): PluginOption {
  return {
    name: "wasp:config",
    enforce: "pre",
    config(config, env) {
      throwIfOverridingForcedOptions(config);

      // Returned config is merged with the user's config by Vite (mergeConfig).
      return {
        base: forcedOptions["base"],
        optimizeDeps: {
          exclude: ["wasp"]
        },
        server: {
          port: forcedOptions["server.port"],
          strictPort: forcedOptions["server.strictPort"],
          host: useUserValue(config.server?.host, "0.0.0.0"),
        },
        preview: {
          port: forcedOptions["preview.port"],
        },
        envPrefix: forcedOptions["envPrefix"],
        define:
          env.mode === "test"
            ? defineMissingClientEnvVars(testModeEnvVarDefaults)
            : {},
        build: {
          outDir: forcedOptions["build.outDir"],
        },
        ssr: {
          noExternal: ["@wasp.sh/lib-sdk-core"],
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
      };
    },
  };
}

function defineMissingClientEnvVars(
  envVars: Record<string, string>,
): Record<string, string> {
  return Object.fromEntries(
    Object.entries(envVars)
      .filter(([name]) => process.env[name] === undefined)
      .map(([name, value]) => [`import.meta.env.${name}`, JSON.stringify(value)]),
  );
}

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
