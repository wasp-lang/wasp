{{={= =}=}}
import * as path from "node:path";
import { type Plugin } from "vite";

const userVirtualModuleMap: Record<string, string> = {
  {=# userVirtualModules =}
  '{= virtualPath =}': '{=& importJson.importPath =}',
  {=/ userVirtualModules =}
};

/**
 * Resolves virtual modules pointing to user's files.
 * User virtual modules allow Wasp code to depend on user code at runtime,
 * without depending on the user's project during compile time.
 */
export function userVirtualModules(): Plugin {
  let rootDir!: string;

  return {
    name: "wasp:user-virtual-modules",
    enforce: "pre",
    configResolved(config) {
      rootDir = config.root;
    },
    async resolveId(id, importer, options) {
      if (id in userVirtualModuleMap) {
        const absPath = path.resolve(rootDir, userVirtualModuleMap[id]);
        return this.resolve(absPath, importer, { ...options, skipSelf: true });
      }
      return null;
    },
  };
}
