{{={= =}=}}
import path from "node:path";
import { type Plugin } from "vite";

/**
 * Maps the name of a virtual module pointing to the user's file to
 * that file's relative import path from the client project root.
 * 
 * @example userVirtualModuleMap["virtual:wasp/user/client-env-schema"] // resolves to "./src/env"
 */
const userVirtualModuleMap: { [userVirtualModule: string]: string } = {
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
      if (id in userVirtualModuleMap) {
        const absPath = path.resolve(clientRootDir, userVirtualModuleMap[id]);
        return this.resolve(absPath, importer, { ...options, skipSelf: true });
      }
      return null;
    },
  };
}
