{{={= =}=}}
import { type Plugin } from "vite";
import {
  getClientEntryTsxContent,
  getRoutesTsxContent,
  getSsrEntryTsxContent,
} from "../virtual-files/index.js";
import { makeVirtualFilesResolver, type VirtualFiles } from "../virtual-files/resolver.js";

const resolveVirtualFiles = makeVirtualFilesResolver([
  { id: "{= clientEntryPointPath =}", load: getClientEntryTsxContent },
  { id: "{= routesEntryPointPath =}", load: getRoutesTsxContent },
  { id: "{= ssrEntryPointPath =}", load: getSsrEntryTsxContent },
]);

export function virtualWaspModules(): Plugin {
  let virtualFiles!: VirtualFiles;

  return {
    name: "wasp:virtual-wasp-modules",
    enforce: "pre",
    configResolved(config) {
      virtualFiles = resolveVirtualFiles(config.root);
    },
    resolveId: (id) => virtualFiles.resolveId(id),
    load: (id) => virtualFiles.load(id),
  };
}
