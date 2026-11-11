{{={= =}=}}
import type { Plugin } from "vite";
import {
  getClientAppTsxContent,
  getClientEntryTsxContent,
  getRoutesTsxContent,
  getSsrEntryTsxContent,
} from "../virtual-files/index.js";
import { makeVirtualFilesResolver, type VirtualFiles } from "@wasp/lib-sdk-core/node/vite";

const resolveVirtualFiles = makeVirtualFilesResolver([
  { id: "{= clientEntryPointPath =}", load: getClientEntryTsxContent },
  { id: "{= clientAppPath =}", load: getClientAppTsxContent },
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
    resolveId: (id) => virtualFiles.ids.get(id),
    load(id) {
      const loader = virtualFiles.loaders.get(id);
      return loader?.();
    },
  };
}
