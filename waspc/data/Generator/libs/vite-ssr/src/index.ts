import type { PluginOption } from "vite";
import { ssrBuild } from "./build";
import { PLUGIN_NAME, type Options } from "./common";
import { ssrDev } from "./dev";
import { ssrPreview } from "./preview";
import { Routes } from "./routes";

export default function ssr(options: Options): PluginOption {
  const routes = new Routes(options.ssrPaths, options.ssrFallbackPath);

  return [
    {
      name: `${PLUGIN_NAME}:set-environments`,
      config: () => ({
        environments: {
          ssr: {},
          client: {},
        },
      }),
    },
    ssrDev(routes, options),
    ssrBuild(routes, options),
    ssrPreview(routes),
  ];
}
