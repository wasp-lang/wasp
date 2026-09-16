import path from "node:path";
/**
 * Maps virtual module IDs (pointing to user's client modules)
 * to their relative import paths from the client project root.
 *
 * @example
 * clientVirtualUserModuleMap["virtual:wasp/user/env"] // => "./src/env"
 */
const clientVirtualUserModuleMap = {};
/**
 * Resolves virtual modules pointing to user's modules.
 * Virtual user modules allow Wasp code to depend on user code at runtime,
 * without depending on the user's project during compile time.
 */
export function virtualUserModules() {
    let clientRootDir;
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
//# sourceMappingURL=virtualUserModules.js.map