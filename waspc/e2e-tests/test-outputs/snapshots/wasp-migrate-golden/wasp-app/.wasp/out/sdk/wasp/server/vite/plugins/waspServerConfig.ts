import path from "node:path";
import { type PluginOption, type UserConfig } from "vite";
import { ENVIRONMENT_NAMES } from "../../../vite/constants.js";

/**
 * Declares the `server` environment: how Wasp's Node.js server is resolved and
 * bundled.
 *
 * Wasp builds this environment with `wasp/scripts/build-server.mjs`, never with
 * `vite build` (which keeps meaning "build the client app").
 */
export function waspServerConfig(): PluginOption {
  return {
    name: "wasp:server-config",
    enforce: "pre",
    config(config) {
      throwIfOverridingForcedOptions(config);

      const waspProjectDir = config.root ?? process.cwd();
      const resolveInWaspProjectDir = (relativePath: string): string =>
        path.resolve(waspProjectDir, relativePath);

      const bundleEntries: Record<string, string> = {
        server: resolveInWaspProjectDir(".wasp/out/server/src/server.ts"),
      };

      // Returned config is merged with the user's config by Vite (mergeConfig).
      return {
        environments: {
          [ENVIRONMENT_NAMES.SERVER]: {
            // The server code runs in Node.js, not in a browser.
            consumer: "server",
            // The server reads its configuration from `process.env` at runtime,
            // so Vite must not replace `process.env.*` with static values.
            keepProcessEnv: true,
            optimizeDeps: {
              // Dependency pre-bundling only benefits the browser.
              noDiscovery: true,
              include: [],
            },
            resolve: {
              // Everything that resolves to `node_modules` stays external and
              // is loaded by Node.js at runtime. The Wasp SDK is the exception:
              // it must be bundled because it uses extensionless relative
              // imports, which Node.js can't resolve.
              // See https://github.com/wasp-lang/wasp/issues/2492.
              noExternal: ["wasp"],
            },
            build: {
              outDir: resolveInWaspProjectDir(".wasp/out/server/bundle/"),
              emptyOutDir: true,
              // The bundle only ever runs in Node.js, so we don't have to care
              // about browser support.
              target: "esnext",
              // Wasp runs the server with `--enable-source-maps`.
              sourcemap: true,
              // Vite copies the project's `public` dir into the build output,
              // which only makes sense for the client build.
              copyPublicDir: false,
              rolldownOptions: {
                input: bundleEntries,
                output: {
                  format: "es",
                  entryFileNames: "[name].js",
                },
              },
            },
          },
        },
      };
    },
  };
}

/**
 * Options Wasp fully controls. Unlike the client plugin's forced options, their
 * values depend on the Wasp project's directory, so we only check whether the
 * user set them at all.
 */
function throwIfOverridingForcedOptions(config: UserConfig): void {
  const serverBuildConfig = config.environments?.[ENVIRONMENT_NAMES.SERVER]?.build;
  const conflicts = [
    serverBuildConfig?.outDir !== undefined && "build.outDir",
    serverBuildConfig?.rolldownOptions?.input !== undefined &&
      "build.rolldownOptions.input",
  ].filter((conflict) => typeof conflict === "string");

  if (conflicts.length > 0) {
    const conflictPaths = conflicts.map(
      (conflict) => `  - "environments.${ENVIRONMENT_NAMES.SERVER}.${conflict}"`,
    );
    throw new Error(
      `Your vite.config.ts sets options that Wasp controls:\n${conflictPaths.join("\n")}\n\nRemove these from your Vite config, Wasp sets them automatically.`,
    );
  }
}
