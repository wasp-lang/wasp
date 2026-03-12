import type { PluginOption } from "vite";
import { ssrBuild } from "./build";
import type { Options } from "./common/options";
import { Routes } from "./common/routes";
import { ssrDev } from "./dev";
import { ssrPreview } from "./preview";
import { ssrSetEnvironments } from "./set-environments";

export function ssr(options: Options): PluginOption {
  const routes = new Routes(options.ssrPaths, options.ssrFallbackPath);

  return [
    ssrSetEnvironments(),
    ssrDev(routes, options),
    ssrBuild(routes, options),
    ssrPreview(routes),
  ];
}
