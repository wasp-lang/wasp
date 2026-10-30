import fs from "fs";
import path from "node:path";
import { assertDirExists, assertDirPathIsAbsolute } from "./validation.js";
import { createCommandWithCwd } from "./zx.js";
export async function assertValidWaspProject(waspProjectDir, waspExe) {
    const waspCli = createCommandWithCwd(waspExe, waspProjectDir);
    const result = await waspCli(["show", "spec", "--json"], {
        quiet: true,
        nothrow: true,
    });
    if (result.exitCode !== 0) {
        throw new Error([
            "The supplied Wasp directory does not appear to be a valid Wasp project.",
            "Please double check your Wasp project directory.",
        ].join("\n"));
    }
}
export function assertWaspProjectDirIsAbsoluteAndPresent(waspProjectDir) {
    const dirNameInError = "Wasp project directory";
    assertDirPathIsAbsolute(waspProjectDir, dirNameInError);
    assertDirExists(waspProjectDir, dirNameInError);
}
export function buildDirExists(waspProjectDir) {
    return fs.existsSync(getWaspBuildDir(waspProjectDir));
}
export function getServerBuildArtefactsDir(waspProjectDir) {
    return getServerDeploymentDir(waspProjectDir);
}
export function getClientBuildArtefactsDir(waspProjectDir) {
    return path.join(getClientDeploymentDir(waspProjectDir), "build");
}
function getWaspBuildDir(waspProjectDir) {
    return path.join(waspProjectDir, ".wasp", "out");
}
export function getServerDeploymentDir(waspProjectDir) {
    // The server is built from the Wasp out directory.
    return path.join(getWaspBuildDir(waspProjectDir), ".");
}
// Deployment tooling needs a dir outside of the user project dir where it can
// create files like a Dockerfile for the static server.
export function ensureClientDeploymentDir(waspProjectDir) {
    const clientDeploymentDir = getClientDeploymentDir(waspProjectDir);
    fs.mkdirSync(clientDeploymentDir, { recursive: true });
    return clientDeploymentDir;
}
function getClientDeploymentDir(waspProjectDir) {
    // The client is deployed from the `.out/web-app` dir.
    return path.join(getWaspBuildDir(waspProjectDir), "web-app");
}
export function getClientBuildDir(waspProjectDir) {
    // The client is built from the project root dir.
    return path.join(waspProjectDir, ".");
}
//# sourceMappingURL=waspProject.js.map