import path from "node:path";
import { type Plugin } from "vite";
import { ENVIRONMENT_NAMES } from "../../../vite/constants.js";

/**
 * Maps virtual module IDs (pointing to user's client modules)
 * to their relative import paths from the client project root.
 * 
 * @example
 * clientVirtualUserModuleMap["virtual:wasp/user/env"] // => "./src/env"
 */
const clientVirtualUserModuleMap: { [virtualUserModule: string]: string } = {
  'virtual:wasp/user/env': './src/env',
};

/**
 * Resolves virtual modules pointing to user's modules.
 * Virtual user modules allow Wasp code to depend on user code at runtime,
 * without depending on the user's project during compile time.
 */
export function virtualUserModules(): Plugin {
  let clientRootDir!: string;

  return {
    name: "wasp:virtual-user-modules",
    enforce: "pre",
    // These virtual modules point to the user's client code, so they only make
    // sense in the environments that process it.
    applyToEnvironment: (environment) =>
      environment.name === ENVIRONMENT_NAMES.CLIENT ||
      environment.name === ENVIRONMENT_NAMES.SSR,
    configResolved(config) {
      clientRootDir = config.root;
    },
    async resolveId(id, importer, options) {
      if (Object.hasOwn(clientVirtualUserModuleMap, id)) {
        const absPath = path.resolve(clientRootDir, clientVirtualUserModuleMap[id]);
        return this.resolve(absPath, importer, { ...options, skipSelf: true });
      }
      return null;
    },
  };
}
