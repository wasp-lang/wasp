{{={= =}=}}
import { type Plugin } from "vite";
import {
  getIndexTsxContent,
  getRoutesTsxContent,
  getEntryServerTsxContent,
} from "../virtual-files/index.js";
import { makeVirtualFilesResolver, type VirtualFiles } from "../virtual-files/resolver.js";

const resolveVirtualFiles = makeVirtualFilesResolver([
  { id: "{= clientEntryPointPath =}", load: getIndexTsxContent },
  { id: "{= ssrEntryPointPath =}", load: getEntryServerTsxContent },
  { id: "{= routesEntryPointPath =}", load: getRoutesTsxContent },
]);

export function waspVirtualModules(): Plugin {
  let virtualFiles!: VirtualFiles;

  return {
    name: "wasp:virtual-modules",
    enforce: "pre",
    configResolved(config) {
      virtualFiles = resolveVirtualFiles(config.root);
    },
    resolveId: (id) => virtualFiles.ids.get(id),
    load(id) {
      const loader = virtualFiles.loaders.get(id);
      return loader?.();
    },
  };
}
