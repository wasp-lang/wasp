import * as path from "node:path";
import type { Plugin } from "vite";
import type { GeneratedViteConfig } from "../generatedViteConfig.js";

/**
 * Resolves virtual modules pointing to user's modules.
 * Virtual user modules allow Wasp code to depend on user code at runtime,
 * without depending on the user's project during compile time.
 */
export function virtualUserModules(
  virtualUserModuleMap: GeneratedViteConfig["virtualUserModules"],
): Plugin {
  let clientRootDir!: string;

  return {
    name: "wasp:virtual-user-modules",
    enforce: "pre",
    configResolved(config) {
      clientRootDir = config.root;
    },
    async resolveId(id, importer, options) {
      const userModulePath = Object.hasOwn(virtualUserModuleMap, id)
        ? virtualUserModuleMap[id]
        : undefined;
      if (userModulePath !== undefined) {
        const absPath = path.resolve(clientRootDir, userModulePath);
        return this.resolve(absPath, importer, { ...options, skipSelf: true });
      }
      return null;
    },
  };
}
