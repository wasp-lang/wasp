{{={= =}=}}
import path from "node:path";
import { type Plugin } from "vite";

/**
 * Maps virtual module IDs (pointing to user's client modules)
 * to their relative import paths from the client project root.
 * 
 * @example
 * clientVirtualUserModuleMap["virtual:wasp/user/env"] // => "./src/env"
 */
const clientVirtualUserModuleMap: { [virtualUserModule: string]: string } = {
  {=# virtualUserModules =}
  '{= virtualModuleId =}': '{=& importJson.importPath =}',
  {=/ virtualUserModules =}
};

/**
 * Resolves virtual modules pointing to user's modules.
 * Virtual user modules allow Wasp code to depend on user code at runtime,
 * without depending on the user's project during compile time.
 */
export function waspVirtualUserModules(): Plugin {
  let clientRootDir!: string;

  return {
    name: "wasp:virtual-user-modules",
    enforce: "pre",
    configResolved(config) {
      clientRootDir = config.root;
    },
    async resolveId(id, importer, options) {
      if (id in clientVirtualUserModuleMap) {
        const absPath = path.resolve(clientRootDir, clientVirtualUserModuleMap[id]);
        return this.resolve(absPath, importer, { ...options, skipSelf: true });
      }
      return null;
    },
  };
}
