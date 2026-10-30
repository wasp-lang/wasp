import { getFullCommandName } from "../../../../common/commander.js";
import { waspSays } from "../../../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../../../common/waspBuild.js";
import { createDeploymentInstructions } from "../../DeploymentInstructions.js";
import { railwaySetupCommand } from "../../index.js";
import { linkRailwayProjectToWaspProjectDir } from "../../railwayProject/cli.js";
import { getRailwayProjectStatus, ProjectStatus, } from "../../railwayProject/index.js";
import { deployClient } from "./client.js";
import { deployServer } from "./server.js";
export async function deploy(projectName, options) {
    const deploymentInstructions = createDeploymentInstructions(projectName, options);
    await ensureRailwayProjectForDirectory({
        projectName,
        waspProjectDir: options.waspProjectDir,
        existingProjectId: options.existingProjectId,
        railwayExe: options.railwayExe,
    });
    waspSays("Deploying your Wasp app to Railway!");
    await ensureWaspProjectIsBuilt(options);
    if (options.skipServer) {
        waspSays("Skipping server deploy due to CLI option.");
    }
    else {
        await deployServer(deploymentInstructions);
    }
    if (options.skipClient) {
        waspSays("Skipping client deploy due to CLI option.");
    }
    else {
        await deployClient(deploymentInstructions);
    }
}
async function ensureRailwayProjectForDirectory({ railwayExe, projectName, waspProjectDir, existingProjectId, }) {
    const { status, project } = await getRailwayProjectStatus({
        projectName,
        waspProjectDir,
        railwayExe,
        existingProjectId,
    });
    switch (status) {
        case ProjectStatus.EXISTING_PROJECT_ALREADY_LINKED:
            waspSays(`Using already linked project: "${projectName}"`);
            return project;
        case ProjectStatus.EXISTING_PROJECT_SHOULD_BE_LINKED:
            waspSays(`Linking Railway project with name "${project.name}" to project directory.`);
            return linkRailwayProjectToWaspProjectDir(project, {
                railwayExe,
                waspProjectDir,
            });
        case ProjectStatus.MISSING_PROJECT:
            throw new Error(`No Railway project found. Run ${getFullCommandName(railwaySetupCommand)} first or provide an existing project ID with "--existing-project-id" option.`);
        default:
            status;
            throw new Error(`Unhandled status: ${status}`);
    }
}
//# sourceMappingURL=index.js.map