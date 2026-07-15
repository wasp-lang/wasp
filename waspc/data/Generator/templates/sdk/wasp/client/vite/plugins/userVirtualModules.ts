{{={= =}=}}
import path from "node:path";
import { type Plugin } from "vite";

/**
 * Maps user virtual module IDs to their relative import paths from
 * the client project root.
 * 
 * @example
 * clientUserVirtualModuleMap["virtual:wasp/user/client-env-schema"] // => "./src/env"
 */
const clientUserVirtualModuleMap: { [userVirtualModule: string]: string } = {
  {=# userVirtualModules =}
  '{= virtualModuleId =}': '{=& importJson.importPath =}',
  {=/ userVirtualModules =}
};

/**
 * Resolves virtual modules pointing to user's files.
 * User virtual modules allow Wasp code to depend on user code at runtime,
 * without depending on the user's project during compile time.
 */
export function userVirtualModules(): Plugin {
  let clientRootDir!: string;

  return {
    name: "wasp:user-virtual-modules",
    enforce: "pre",
    configResolved(config) {
      clientRootDir = config.root;
    },
    async resolveId(id, importer, options) {
      if (id in clientUserVirtualModuleMap) {
        const absPath = path.resolve(clientRootDir, clientUserVirtualModuleMap[id]);
        return this.resolve(absPath, importer, { ...options, skipSelf: true });
      }
      return null;
    },
  };
}
