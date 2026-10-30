import fs from "fs";
import path from "node:path";
import toml from "toml";
import { assertDirExists, assertDirPathIsAbsolute, } from "../../common/validation.js";
export function getTomlFilePaths(cmdOptions) {
    const baseDir = cmdOptions.flyTomlDir || cmdOptions.waspProjectDir;
    return {
        serverTomlPath: path.join(baseDir, "fly-server.toml"),
        clientTomlPath: path.join(baseDir, "fly-client.toml"),
    };
}
export function serverTomlExistsInProject(paths) {
    return fs.existsSync(paths.serverTomlPath);
}
export function copyLocalServerTomlToProject(paths) {
    fs.copyFileSync("fly.toml", paths.serverTomlPath);
}
export function copyProjectServerTomlLocally(paths) {
    fs.copyFileSync(paths.serverTomlPath, "fly.toml");
}
export function clientTomlExistsInProject(paths) {
    return fs.existsSync(paths.clientTomlPath);
}
export function copyLocalClientTomlToProject(paths) {
    fs.copyFileSync("fly.toml", paths.clientTomlPath);
}
export function copyProjectClientTomlLocally(paths) {
    fs.copyFileSync(paths.clientTomlPath, "fly.toml");
}
export function localTomlExists() {
    return fs.existsSync("fly.toml");
}
export function deleteLocalToml() {
    if (localTomlExists()) {
        fs.unlinkSync("fly.toml");
    }
}
export function getAppNameFromToml(path) {
    const content = fs.readFileSync(path, "utf8");
    const data = toml.parse(content);
    return data.app;
}
export function getInferredBasenameFromServerToml(paths) {
    const serverFlyAppName = getAppNameFromToml(paths.serverTomlPath);
    return inferServerBasename(serverFlyAppName);
}
export function getInferredBasenameFromClientToml(paths) {
    const clientFlyAppName = getAppNameFromToml(paths.clientTomlPath);
    return inferClientBasename(clientFlyAppName);
}
export function inferServerBasename(name) {
    return name.replace(/-server$/, "");
}
export function inferClientBasename(name) {
    return name.replace(/-client$/, "");
}
export function replaceLineInLocalToml(searchValue, replaceValue) {
    const content = fs.readFileSync("fly.toml", "utf8");
    const updatedContent = content.replace(searchValue, replaceValue);
    fs.writeFileSync("fly.toml", updatedContent);
}
export function doesLocalTomlContainLine(searchValue) {
    const content = fs.readFileSync("fly.toml", "utf8");
    return content.search(searchValue) !== -1;
}
export function assertFlyTomlDirIsAbsoluteAndPresent(flyTomlDirPath) {
    const dirNameInError = "TOML directory";
    assertDirPathIsAbsolute(flyTomlDirPath, dirNameInError);
    assertDirExists(flyTomlDirPath, dirNameInError);
}
//# sourceMappingURL=tomlFile.js.map