{{={= =}=}}
import path from "node:path";
import { fileURLToPath } from "node:url";

const serverRootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");

const userVirtualModuleMap = {
  {=# userVirtualModules =}
  "{= virtualPath =}": "{=& importJson.importPath =}",
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
      if (id in userVirtualModuleMap) {
        const absPath = path.resolve(serverRootDir, userVirtualModuleMap[id]);
        return await this.resolve(absPath, undefined, { skipSelf: true });
      }
      return null;
    },
  };
}
