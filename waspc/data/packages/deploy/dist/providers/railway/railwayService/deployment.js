import { setTimeout } from "node:timers/promises";
import { waspSays } from "../../../common/terminal.js";
import { createCommandWithCwd, tryRunJsonCommand } from "../../../common/zx.js";
import { RailwayCliServiceStatusSchema, } from "../jsonOutputSchemas.js";
const POLL_INTERVAL_MS = 5000;
const TIMEOUT_MS = 5 * 60 * 1000;
// Any other status means the deployment is still in progress.
const SUCCESS_STATUS = "SUCCESS";
const FAILURE_STATUSES = ["FAILED", "CRASHED"];
export async function waitForServiceDeploymentSuccess(service, options) {
    const deadline = Date.now() + TIMEOUT_MS;
    while (Date.now() < deadline) {
        const status = await getLatestServiceDeploymentStatus(service, options);
        if (status === SUCCESS_STATUS) {
            return;
        }
        if (status !== null && FAILURE_STATUSES.includes(status)) {
            throw new Error(`"${service.name}" deployment finished with status "${status}". Check the Railway dashboard for details.`);
        }
        waspSays(`Waiting for "${service.name}" deployment... (Status: "${status ?? "UNKNOWN"}")`);
        await setTimeout(POLL_INTERVAL_MS);
    }
    throw new Error(`Timed out waiting for "${service.name}" to be deployed. Check the Railway dashboard for details.`);
}
async function getLatestServiceDeploymentStatus(service, options) {
    const railwayCli = createCommandWithCwd(options.railwayExe, options.waspProjectDir);
    const serviceStatus = await tryRunJsonCommand(railwayCli, ["service", "status", "--service", service.id, "--json"], RailwayCliServiceStatusSchema);
    // Treat a failed command as a transient Railway CLI failure, i.e. "not
    // ready yet".
    return serviceStatus?.status ?? null;
}
//# sourceMappingURL=deployment.js.map