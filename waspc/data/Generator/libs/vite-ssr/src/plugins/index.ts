import type { PluginOption } from "vite";
import { ssrBuild } from "./build";
import type { Options } from "./common/options";
import { SsrRoutes } from "./common/routes";
import { ssrDev } from "./dev";
import { ssrPreview } from "./preview";
import { ssrSetEnvironments } from "./set-environments";

export function ssr(options: Options): PluginOption {
  const routes = new SsrRoutes(options.ssrPaths, options.ssrFallbackFiles);

  return [
    // We declare the Vite Environments we're using (ssr and client) in a
    // separate plugin, so that they are available in all the other plugins:
    ssrSetEnvironments(),

    // Then we have plugins that only apply in certain modes, and handle the
    // main SSR logic for those modes:
    ssrDev(routes, options),
    ssrBuild(routes, options),
    ssrPreview(routes),
  ];
}
