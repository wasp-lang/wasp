import { cd } from "zx";
import { ensureClientDeploymentDir, getServerDeploymentDir, } from "../../common/waspProject.js";
import { clientTomlExistsInProject, copyLocalClientTomlToProject, copyLocalServerTomlToProject, copyProjectClientTomlLocally, copyProjectServerTomlLocally, serverTomlExistsInProject, } from "./tomlFile.js";
export var ContextOption;
(function (ContextOption) {
    ContextOption["Client"] = "client";
    ContextOption["Server"] = "server";
})(ContextOption || (ContextOption = {}));
export function getCommonOps(context, waspProjectDir, paths) {
    const commonOps = {
        [ContextOption.Client]: createClientCommonOps(waspProjectDir, paths),
        [ContextOption.Server]: createServerCommonOps(waspProjectDir, paths),
    };
    return commonOps[context];
}
function createClientCommonOps(waspProjectDir, paths) {
    return {
        waspProjectDir,
        paths,
        cdToDeploymentDir: () => cd(ensureClientDeploymentDir(waspProjectDir)),
        tomlExistsInProject: () => clientTomlExistsInProject(paths),
        copyLocalTomlToProject: () => copyLocalClientTomlToProject(paths),
        copyProjectTomlLocally: () => copyProjectClientTomlLocally(paths),
    };
}
function createServerCommonOps(waspProjectDir, paths) {
    return {
        waspProjectDir,
        paths,
        cdToDeploymentDir: () => cd(getServerDeploymentDir(waspProjectDir)),
        tomlExistsInProject: () => serverTomlExistsInProject(paths),
        copyLocalTomlToProject: () => copyLocalServerTomlToProject(paths),
        copyProjectTomlLocally: () => copyProjectServerTomlLocally(paths),
    };
}
//# sourceMappingURL=CommonOps.js.map