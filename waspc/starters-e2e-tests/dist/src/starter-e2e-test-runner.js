import { fileURLToPath } from "url";
import { $, chalk, fs, path, spinner, tmpdir, within } from "zx";
import { setupWaspMailCrabConfiguration } from "./mailcrab.js";
export async function runStarterE2ETests(execution) {
    const { waspCliCommand, starterE2ETests } = execution;
    const { starterName } = starterE2ETests;
    const waspProjectPath = await spinner("Initializing test environment...", () => initializeTestEnvironment(execution));
    // TODO: implement branching logic based on included tests or not
    await runDevE2ETests(starterName, waspProjectPath, waspCliCommand);
    await runBuildE2ETests(starterName, waspProjectPath, waspCliCommand);
}
async function initializeTestEnvironment(execution) {
    const { waspCliCommand, starterE2ETests } = execution;
    const { starterName, waspProjectRelativePath } = starterE2ETests;
    const tempDirectoryPath = tmpdir();
    process.on("exit", () => cleanup(tempDirectoryPath));
    process.on("SIGINT", () => cleanup(tempDirectoryPath));
    process.on("SIGTERM", () => cleanup(tempDirectoryPath));
    process.on("SIGQUIT", () => cleanup(tempDirectoryPath));
    const waspStarterProjectName = `wasp-starter-${starterName}`;
    await $({
        cwd: tempDirectoryPath,
    }) `${waspCliCommand} new ${waspStarterProjectName} -t ${starterName}`;
    const waspProjectPath = path.join(tempDirectoryPath, waspStarterProjectName, waspProjectRelativePath);
    await initializeServerEnvironment(waspProjectPath);
    await initializeClientEnvironment(waspProjectPath);
    await setupWaspMailCrabConfiguration(waspProjectPath);
    return waspProjectPath;
}
async function cleanup(tempDirectoryPath) {
    await $({ nothrow: true }) `rm -rf ${tempDirectoryPath}`;
}
async function initializeServerEnvironment(waspProjectPath) {
    const serverEnvFileExamplePath = path.join(waspProjectPath, ".env.server.example");
    const serverEnvFilePath = path.join(waspProjectPath, ".env.server");
    if (await fs.pathExists(serverEnvFileExamplePath)) {
        await fs.copy(serverEnvFileExamplePath, serverEnvFilePath);
    }
    else {
        await fs.ensureFile(serverEnvFilePath);
    }
    await fs.appendFile(serverEnvFilePath, "\nSKIP_EMAIL_VERIFICATION_IN_DEV=true\n");
}
async function initializeClientEnvironment(waspProjectPath) {
    const clientEnvFileExamplePath = path.join(waspProjectPath, ".env.client.example");
    const clientEnvFilePath = path.join(waspProjectPath, ".env.client");
    if (await fs.pathExists(clientEnvFileExamplePath)) {
        await fs.copy(clientEnvFileExamplePath, clientEnvFilePath);
    }
}
async function runDevE2ETests(templateName, waspProjectPath, waspCliCommand) {
    console.log(`Running DEV e2e tests for ${templateName} starter...`);
    const waspAppRunnerDevEnv = {
        ...process.env,
        WASP_APP_PATH: waspProjectPath,
        WASP_CLI_CMD: waspCliCommand,
        WASP_RUN_MODE: "dev",
    };
    await $({
        env: waspAppRunnerDevEnv,
        stdio: "inherit",
    }) `npx playwright test --grep ${getStarterPlaywrightGrepRegex(templateName)}`;
}
async function runBuildE2ETests(templateName, tempWaspProjectPath, waspCliCommand) {
    if (await waspProjectUsesSqlite(tempWaspProjectPath, waspCliCommand)) {
        console.log(chalk.yellow(`Skipping BUILD e2e tests for ${templateName} starter - uses SQLite`));
        return;
    }
    console.log(`Running BUILD e2e tests for ${templateName} starter...`);
    const waspAppRunnerBuildEnv = {
        ...process.env,
        WASP_APP_PATH: tempWaspProjectPath,
        WASP_CLI_CMD: waspCliCommand,
        WASP_RUN_MODE: "build",
    };
    await $({
        env: waspAppRunnerBuildEnv,
        stdio: "inherit",
    }) `npx playwright test --grep ${getStarterPlaywrightGrepRegex(templateName)}`;
}
/**
 * We search for Playwright tests with either:
 *  - tag for that starter template
 *  - no tag at all
 */
function getStarterPlaywrightGrepRegex(templateName) {
    return `(@${templateName}|^(?!.*@).*)`;
}
async function waspProjectUsesSqlite(tempWaspProjectPath, waspCliCommand) {
    const scriptDirectory = path.dirname(fileURLToPath(import.meta.url));
    const databaseProviderScriptPath = path.join(scriptDirectory, "../../../../scripts/get-wasp-database-provider.sh");
    return await within(async () => {
        $.cwd = tempWaspProjectPath;
        const result = await $ `${databaseProviderScriptPath} ${waspCliCommand}`;
        return result.stdout.trim() === "sqlite";
    });
}
