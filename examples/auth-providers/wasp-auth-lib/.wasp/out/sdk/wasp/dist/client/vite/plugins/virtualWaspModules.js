import { getClientEntryTsxContent, getRoutesTsxContent, getSsrEntryTsxContent, } from "../virtual-files/index.js";
import { makeVirtualFilesResolver } from "../virtual-files/resolver.js";
const resolveVirtualFiles = makeVirtualFilesResolver([
    { id: "/@wasp/client-entry.tsx", load: getClientEntryTsxContent },
    { id: "/@wasp/routes.tsx", load: getRoutesTsxContent },
    { id: "/@wasp/ssr-entry.tsx", load: getSsrEntryTsxContent },
]);
export function virtualWaspModules() {
    let virtualFiles;
    return {
        name: "wasp:virtual-wasp-modules",
        enforce: "pre",
        configResolved(config) {
            virtualFiles = resolveVirtualFiles(config.root);
        },
        resolveId: (id) => virtualFiles.ids.get(id),
        load(id) {
            const loader = virtualFiles.loaders.get(id);
            return loader?.();
        },
    };
}
//# sourceMappingURL=virtualWaspModules.js.map