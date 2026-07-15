{{={= =}=}}
import path from "node:path";
import { fileURLToPath } from "node:url";

const serverRootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");

/**
 * Maps user virtual module IDs to their relative import paths from
 * the server project root.
 * 
 * @example 
 * serverUserVirtualModuleMap["virtual:wasp/user/server-env-schema"] // => "../../../src/env"
 */
const serverUserVirtualModuleMap = {
  {=# userVirtualModules =}
  "{= virtualModuleId =}": "{=& importJson.importPath =}",
  {=/ userVirtualModules =}
};

/**
 * Resolves virtual modules pointing to user's files.
 * User virtual modules allow Wasp code to depend on user code at runtime,
 * without depending on the user's project during compile time.
 */
export function userVirtualModules() {
  return {
    name: "wasp:user-virtual-modules",
    async resolveId(id) {
      if (id in serverUserVirtualModuleMap) {
        const absPath = path.resolve(serverRootDir, serverUserVirtualModuleMap[id]);
        return await this.resolve(absPath, undefined, { skipSelf: true });
      }
      return null;
    },
  };
}
