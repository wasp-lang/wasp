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
  "virtual:wasp/user/env": "../../../src/env",
  "virtual:wasp/user/features/db/prisma": "../../../src/features/db/prisma",
  "virtual:wasp/user/features/operations/queries": "../../../src/features/operations/queries",
  "virtual:wasp/user/features/operations/queries": "../../../src/features/operations/queries",
  "virtual:wasp/user/features/operations/queries": "../../../src/features/operations/queries",
  "virtual:wasp/user/features/operations/getOldestTask": "../../../src/features/operations/getOldestTask",
  "virtual:wasp/user/features/operations/queries": "../../../src/features/operations/queries",
  "virtual:wasp/user/features/jobs/uppercaseText": "../../../src/features/jobs/uppercaseText",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/features/auth/customSignup": "../../../src/features/auth/customSignup",
  "virtual:wasp/user/features/operations/actions": "../../../src/features/operations/actions",
  "virtual:wasp/user/features/operations/actions": "../../../src/features/operations/actions",
  "virtual:wasp/user/features/operations/actions": "../../../src/features/operations/actions",
  "virtual:wasp/user/features/operations/actions": "../../../src/features/operations/actions",
  "virtual:wasp/user/features/jobs/uppercaseText": "../../../src/features/jobs/uppercaseText",
  "virtual:wasp/user/rpcTests/operations/server": "../../../src/rpcTests/operations/server",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/definitions": "../../../src/rpcTests/operations/definitions",
  "virtual:wasp/user/rpcTests/operations/jsDefinitions": "../../../src/rpcTests/operations/jsDefinitions",
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
