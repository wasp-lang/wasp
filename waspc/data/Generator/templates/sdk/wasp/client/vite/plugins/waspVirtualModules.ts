{{={= =}=}}
import { type Plugin } from "vite";
import {
  getClientEntryTsxContent,
  getRoutesTsxContent,
  getSsrEntryTsxContent,
} from "../virtual-files/index.js";
import { makeVirtualFilesResolver, type VirtualFiles } from "../virtual-files/resolver.js";

const resolveVirtualFiles = makeVirtualFilesResolver([
  { id: "{= clientEntryPointVMId =}", load: getClientEntryTsxContent },
  { id: "{= routesEntryPointVMId =}", load: getRoutesTsxContent },
  { id: "{= ssrEntryPointVMId =}", load: getSsrEntryTsxContent },
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
