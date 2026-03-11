{{={= =}=}}
import * as path from "node:path";
import { type Plugin } from "vite";
import {
  getIndexTsxContent,
  getRoutesTsxContent,
} from "../virtual-files/index.js";
import { makeVirtualFilesResolver, type VirtualFiles } from "../virtual-files/resolver.js";

const resolveVirtualFiles = makeVirtualFilesResolver([
  { id: "{= clientEntryPointPath =}", load: getIndexTsxContent },
  { id: "{= routesEntryPointPath =}", load: getRoutesTsxContent },
]);

{=# hasDirectVirtualModules =}
const directVirtualModuleMap: Record<string, string> = {
  {=# directVirtualModules =}
  '{= virtualPath =}': '{=& importJson.importPath =}',
  {=/ directVirtualModules =}
};
{=/ hasDirectVirtualModules =}

export function waspVirtualModules(): Plugin {
  let virtualFiles!: VirtualFiles;
  let rootDir!: string;

  return {
    name: "wasp:virtual-modules",
    enforce: "pre",
    configResolved(config) {
      virtualFiles = resolveVirtualFiles(config.root);
      rootDir = config.root;
    },
    async resolveId(id, importer, options) {
      {=# hasDirectVirtualModules =}
      if (id in directVirtualModuleMap) {
        const absPath = path.resolve(rootDir, directVirtualModuleMap[id]);
        return this.resolve(absPath, importer, { ...options, skipSelf: true });
      }
      {=/ hasDirectVirtualModules =}
      return virtualFiles.ids.get(id);
    },
    load(id) {
      const loader = virtualFiles.loaders.get(id);
      return loader?.();
    },
  };
}
