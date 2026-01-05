{{={= =}=}}
import path from "node:path";
import { type Plugin } from "vite";
import { getIndexTsxContent } from "./virtual-files/index.virtual.js";
import { getRoutesTsxContent } from "./virtual-files/routes.virtual.js";

const INDEX_PATH = "{= indexVirtualFileName =}";
const ROUTES_PATH = "{= routesVirtualFileName =}";

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
      indexTsxPath = path.resolve(projectRoot, INDEX_PATH);
      routesPath = path.resolve(projectRoot, ROUTES_PATH);
    },
    resolveId(id) {
      // Intercept requests for /index.wasp.tsx and resolve to virtual module
      if (id === INDEX_PATH) {
        return indexTsxPath;
      }
      // Intercept requests for routes.wasp.tsx
      if (id === ROUTES_PATH) {
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
