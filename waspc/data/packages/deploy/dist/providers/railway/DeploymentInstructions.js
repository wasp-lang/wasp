import { createRailwayClientServiceName, createRailwayDbServiceName, createRailwayServerServiceName, } from "./railwayService/nameGenerator.js";
export function createDeploymentInstructions(projectName, cmdOptions) {
    return Object.freeze({
        projectName,
        cmdOptions,
        clientServiceName: createRailwayClientServiceName(projectName),
        serverServiceName: createRailwayServerServiceName(projectName),
        dbServiceName: createRailwayDbServiceName(),
    });
}
//# sourceMappingURL=DeploymentInstructions.js.map