import type { PluginOption } from "vite";
import type { GeneratedViteConfig } from "../generatedViteConfig.js";

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

type ForcedOptions = ReturnType<typeof getForcedOptions>;

function getForcedOptions(generatedConfig: GeneratedViteConfig) {
  return {
    base: generatedConfig.baseDir,
    envPrefix: "REACT_APP_",
    "build.outDir": generatedConfig.clientBuildDir,
    // Heads up! The env referred to by `clientPortEnvVarName` is empty during
    // `build`, so it's not persisted in the final output.
    "server.port": envVarAsNumber(generatedConfig.clientPortEnvVarName),
    "server.strictPort": true,
    // `vite preview` falls back to `server` for most options, but not for `port`
    // (it has its own default), so we have to set it separately.
    "preview.port": envVarAsNumber(generatedConfig.clientPortEnvVarName),
  } as const;
}

const forcedOptionHints: Partial<Record<keyof ForcedOptions, string>> = {
  base: "To serve your app from a subdirectory, set `client.baseDir` in your Wasp config.",
  "server.port":
    "To run the client on a different port, use `wasp start --client-port <port>`.",
  "preview.port":
    "To run the client on a different port, use `wasp build start --client-port <port>`.",
};

export function waspConfig(generatedConfig: GeneratedViteConfig): PluginOption {
  const forcedOptions = getForcedOptions(generatedConfig);

  return {
    name: "wasp:config",
    enforce: "pre",
    config(config) {
      throwIfOverridingForcedOptions(config, forcedOptions);

      // Returned config is merged with the user's config by Vite (mergeConfig).
      return {
        base: forcedOptions["base"],
        optimizeDeps: {
          exclude: generatedConfig.depsExcludedFromOptimization,
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
        build: {
          outDir: forcedOptions["build.outDir"],
        },
        ssr: {
          noExternal: ["@wasp/sdk-core"],
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
        },
      };
    },
  };
}

function useUserValue<T>(userValue: T | undefined, defaultValue: T): T {
  return userValue ?? defaultValue;
}

function throwIfOverridingForcedOptions(
  config: Record<string, any>,
  forcedOptions: ForcedOptions,
): void {
  const conflicts: string[] = [];
  for (const [path, forcedValue] of Object.entries(forcedOptions)) {
    const userValue = getByPath(config, path);
    if (userValue !== undefined && userValue !== forcedValue) {
      const hint = forcedOptionHints[path as keyof ForcedOptions];
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
