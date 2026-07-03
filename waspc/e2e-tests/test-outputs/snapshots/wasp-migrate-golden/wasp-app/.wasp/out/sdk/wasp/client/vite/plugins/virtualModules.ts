import { type Plugin } from "vite";
import {
  getClientEntryTsxContent,
  getRoutesTsxContent,
  getSsrEntryTsxContent,
} from "../virtual-files/index.js";
import { makeVirtualFilesResolver, type VirtualFiles } from "../virtual-files/resolver.js";

const resolveVirtualFiles = makeVirtualFilesResolver([
  { id: "/@wasp/client-entry.tsx", load: getClientEntryTsxContent },
  { id: "/@wasp/routes.tsx", load: getRoutesTsxContent },
  { id: "/@wasp/ssr-entry.tsx", load: getSsrEntryTsxContent },
]);

export function waspVirtualModules(): Plugin {
  let virtualFiles!: VirtualFiles;

  return {
    name: "wasp:virtual-modules",
    enforce: "pre",
    configResolved(config) {
      virtualFiles = resolveVirtualFiles(config.root);
    },
    // We resolve virtual ids to their fake on-disk path, and we also claim
    // that fake path itself. The latter is required because tooling (e.g. the
    // React Fast Refresh wrapper on Vite 8) can re-import a module by its
    // resolved id, and that fake path must stay resolvable back to us.
    resolveId: (id) =>
      virtualFiles.ids.get(id) ?? (virtualFiles.loaders.has(id) ? id : undefined),
    load(id) {
      const loader = virtualFiles.loaders.get(id);
      return loader?.();
    },
  };
}
