import { waspSays } from "../../../../common/terminal.js";
import { retryOnRailwayAPIError } from "../../retry.js";
import { deploy } from "../deploy/index.js";
import { setup } from "../setup/setup.js";
export async function launch(projectName, options) {
    waspSays("Launching your Wasp app to Railway!");
    await retryOnRailwayAPIError(() => setup(projectName, options));
    await retryOnRailwayAPIError(() => deploy(projectName, options));
}
//# sourceMappingURL=launch.js.map