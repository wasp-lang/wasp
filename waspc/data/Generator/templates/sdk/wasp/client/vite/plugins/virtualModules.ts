{{={= =}=}}
import path from "node:path";
import { type Plugin } from "vite";
import {
  getIndexTsxContent,
  getRoutesTsxContent,
} from "../virtual-files/index.js";

const virtualModules = {
  clientEntryPoint: {
    id: "{= clientEntryPointPath =}",
    absPath: "",
    getContent: () => getIndexTsxContent(),
  },
  routesEntryPoint: {
    id: "{= routesEntryPointPath =}",
    absPath: "",
    getContent: () =>  getRoutesTsxContent(),
  },
};

export function waspVirtualModules(): Plugin {
  return {
    name: "wasp:virtual-modules",
    enforce: "pre",
    configResolved(config) {
      // Using absolute paths gives proper context for resolving relative imports.
      virtualModules.clientEntryPoint.absPath = path.resolve(config.root, path.basename(virtualModules.clientEntryPoint.id));
      virtualModules.routesEntryPoint.absPath = path.resolve(config.root, path.basename(virtualModules.routesEntryPoint.id));
    },
    resolveId(id) {
      if (id === virtualModules.clientEntryPoint.id) {
        return virtualModules.clientEntryPoint.absPath;
      }
      if (id === virtualModules.routesEntryPoint.id) {
        return virtualModules.routesEntryPoint.absPath;
      }
    },
    load(id) {
      if (id === virtualModules.clientEntryPoint.absPath) {
        return virtualModules.clientEntryPoint.getContent();
      }
      if (id === virtualModules.routesEntryPoint.absPath) {
        return virtualModules.routesEntryPoint.getContent();
      }
    },
  };
}
