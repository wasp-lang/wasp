import { waspSays } from "./terminal.js";
import { createCommandWithCwd } from "./zx.js";
export const ensureWaspProjectIsBuilt = createEnsureWaspProjectIsBuilt();
function createEnsureWaspProjectIsBuilt() {
    async function buildWaspApp({ waspProjectDir, waspExe, }) {
        waspSays("Building your Wasp app...");
        const waspCli = createCommandWithCwd(waspExe, waspProjectDir);
        return waspCli(["build"]);
    }
    // We want to build the Wasp project only once per CLI invocation.
    // Sometimes `ensureWaspProjectIsBuilt` is called multiple times
    // (e.g. in `setup` command, we call it and then again when deploying).
    let cachedWaspBuildResult = undefined;
    return (...params) => {
        if (cachedWaspBuildResult === undefined) {
            cachedWaspBuildResult = buildWaspApp(...params);
        }
        return cachedWaspBuildResult;
    };
}
//# sourceMappingURL=waspBuild.js.map