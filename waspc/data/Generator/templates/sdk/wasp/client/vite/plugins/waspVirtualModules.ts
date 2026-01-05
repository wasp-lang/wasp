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
      console.log("[wasp-virtual-modules] Resolved paths:", { indexTsxPath, routesPath });
    },
    resolveId(id) {
      console.log("[wasp-virtual-modules] resolveId called with:", id);
      // Intercept requests for /index.wasp.tsx and resolve to virtual module
      if (id === INDEX_PATH) {
        console.log("[wasp-virtual-modules] Resolving INDEX_PATH to:", indexTsxPath);
        return indexTsxPath;
      }
      // Intercept requests for routes.wasp.tsx
      if (id === ROUTES_PATH) {
        console.log("[wasp-virtual-modules] Resolving ROUTES_PATH to:", routesPath);
        return routesPath;
      }
      console.log("[wasp-virtual-modules] No match for id:", id);
    },
    load(id) {
      console.log("[wasp-virtual-modules] load called with:", id);
      if (id === indexTsxPath) {
        console.log("[wasp-virtual-modules] Loading index.wasp.tsx");
        const content = getIndexTsxContent();
        console.log("[wasp-virtual-modules] Generated content length:", content.length);
        return { code: content, map: null };
      }
      if (id === routesPath) {
        console.log("[wasp-virtual-modules] Loading routes.wasp.tsx");
        const content = getRoutesTsxContent();
        console.log("[wasp-virtual-modules] Generated content length:", content.length);
        return { code: content, map: null };
      }
      console.log("[wasp-virtual-modules] No match for id in load:", id);
    },
  };
}
