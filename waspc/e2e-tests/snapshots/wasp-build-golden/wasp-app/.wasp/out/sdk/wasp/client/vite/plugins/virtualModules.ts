import path from "node:path";
import { type Plugin } from "vite";
import { getIndexTsxContent } from "./virtual-files/index.virtual.js";
import { getRoutesTsxContent } from "./virtual-files/routes.virtual.js";



export function waspVirtualModules(): Plugin {
  let projectRoot: string;
  let indexTsxPath: string;
  let routesPath: string;

  return {
    name: "wasp-virtual-modules",
    enforce: "pre",
    configResolved(config) {
      projectRoot = config.root;
      // Using absolute paths gives proper context for resolving relative imports.
      indexTsxPath = path.resolve(projectRoot, "index.virtual.tsx");
      routesPath = path.resolve(projectRoot, "routes.virtual.tsx");
    },
    resolveId(id) {
      // Intercept requests for /index.virtual.tsx and resolve to virtual module
      if (id === "/index.virtual.tsx") {
        return indexTsxPath;
      }
      // Intercept requests for ./routes.virtual.tsx
      if (id === "./routes.virtual.tsx") {
        return routesPath;
      }
    },
    load(id) {
      if (id === indexTsxPath) {
        const content = getIndexTsxContent();
        return { code: content, map: null };
      }
      if (id === routesPath) {
        const content = getRoutesTsxContent();
        return { code: content, map: null };
      }
    },
  };
}
