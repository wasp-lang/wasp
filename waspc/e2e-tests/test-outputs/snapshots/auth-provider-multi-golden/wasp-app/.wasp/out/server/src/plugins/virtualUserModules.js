import path from "node:path";
import { fileURLToPath } from "node:url";

const serverRootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");

/**
 * Maps virtual module IDs (pointing to user's server modules)
 * to their relative import paths from the server project root.
 * 
 * @example 
 * serverVirtualUserModuleMap["virtual:wasp/user/env"] // => "../../../src/env"
 */
const serverVirtualUserModuleMap = {
  "virtual:wasp/user/operations": "../../../src/operations",
  "virtual:wasp/user/operations": "../../../src/operations",
  "virtual:wasp/user/operations": "../../../src/operations",
};

/**
 * Resolves virtual modules pointing to user's modules.
 * Virtual user modules allow Wasp code to depend on user code at runtime,
 * without depending on the user's project during compile time.
 */
export function virtualUserModules() {
  return {
    name: "wasp:virtual-user-modules",
    async resolveId(id) {
      if (Object.hasOwn(serverVirtualUserModuleMap, id)) {
        const absPath = path.resolve(serverRootDir, serverVirtualUserModuleMap[id]);
        return await this.resolve(absPath, undefined, { skipSelf: true });
      }
      return null;
    },
  };
}
