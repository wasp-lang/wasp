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
  'virtual:wasp/user/env': './src/env',
  'virtual:wasp/user/features/db/prisma': './src/features/db/prisma',
  'virtual:wasp/user/features/operations/queries': './src/features/operations/queries',
  'virtual:wasp/user/features/operations/getOldestTask': './src/features/operations/getOldestTask',
  'virtual:wasp/user/features/jobs/uppercaseText': './src/features/jobs/uppercaseText',
  'virtual:wasp/user/rpcTests/operations/definitions': './src/rpcTests/operations/definitions',
  'virtual:wasp/user/features/auth/customSignup': './src/features/auth/customSignup',
  'virtual:wasp/user/features/operations/actions': './src/features/operations/actions',
  'virtual:wasp/user/rpcTests/operations/server': './src/rpcTests/operations/server',
  'virtual:wasp/user/rpcTests/operations/jsDefinitions': './src/rpcTests/operations/jsDefinitions',
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
