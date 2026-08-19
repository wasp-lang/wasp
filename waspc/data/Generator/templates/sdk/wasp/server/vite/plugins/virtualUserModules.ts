{{={= =}=}}
import path from "node:path";
import { type Plugin } from "vite";
import { ENVIRONMENT_NAMES } from "../../../vite/constants.js";

/**
 * Maps virtual module IDs (pointing to user's server modules)
 * to their relative import paths from the Wasp project root.
 *
 * @example
 * serverVirtualUserModuleMap["virtual:wasp/user/queries"] // => "./src/queries"
 */
const serverVirtualUserModuleMap: { [virtualUserModule: string]: string } = {
  {=# virtualUserModules =}
  '{=& virtualModuleId =}': '{=& importJson.importPath =}',
  {=/ virtualUserModules =}
};

/**
 * Resolves virtual modules pointing to user's modules.
 * Virtual user modules allow Wasp code to depend on user code at runtime,
 * without depending on the user's project during compile time.
 */
export function virtualUserModules(): Plugin {
  let waspProjectDir!: string;

  return {
    name: "wasp:server-virtual-user-modules",
    enforce: "pre",
    // These virtual modules point to the user's server code, so they only make
    // sense in the environment that processes it.
    applyToEnvironment: (environment) =>
      environment.name === ENVIRONMENT_NAMES.SERVER,
    configResolved(config) {
      waspProjectDir = config.root;
    },
    async resolveId(id, importer, options) {
      if (Object.hasOwn(serverVirtualUserModuleMap, id)) {
        const absPath = path.resolve(
          waspProjectDir,
          serverVirtualUserModuleMap[id],
        );
        return this.resolve(absPath, importer, { ...options, skipSelf: true });
      }
      return null;
    },
  };
}
