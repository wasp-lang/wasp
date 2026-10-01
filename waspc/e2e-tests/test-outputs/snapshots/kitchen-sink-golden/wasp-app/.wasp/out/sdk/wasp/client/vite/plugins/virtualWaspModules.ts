import type { Plugin } from "vite";
import {
  getClientEntryTsxContent,
  getRoutesTsxContent,
  getSsrEntryTsxContent,
} from "../virtual-files/index.js";
import { makeVirtualFilesResolver, type VirtualFiles } from "@wasp.sh/lib-sdk-core/node/vite";

const resolveVirtualFiles = makeVirtualFilesResolver([
  { id: "/@wasp/client-entry.tsx", load: getClientEntryTsxContent },
  { id: "/@wasp/routes.tsx", load: getRoutesTsxContent },
  { id: "/@wasp/ssr-entry.tsx", load: getSsrEntryTsxContent },
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
