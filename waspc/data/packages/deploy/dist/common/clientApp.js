import { waspSays } from "./terminal.js";
import { getClientBuildArtefactsDir, getClientBuildDir, } from "./waspProject.js";
import { createCommandWithCwd } from "./zx.js";
const serverUrlEnvVarName = "REACT_APP_API_URL";
export async function buildClient(serverUrl, { waspProjectDir }) {
    waspSays("Building web client for production...");
    const clientBuildDir = getClientBuildDir(waspProjectDir);
    const npmCli = createCommandWithCwd("npm", clientBuildDir);
    const npxCli = createCommandWithCwd("npx", clientBuildDir);
    await npmCli(["install"]);
    await npxCli(["vite", "build"], {
        env: {
            ...process.env,
            [serverUrlEnvVarName]: serverUrl,
        },
    });
    return getClientBuildArtefactsDir(waspProjectDir);
}
//# sourceMappingURL=clientApp.js.map