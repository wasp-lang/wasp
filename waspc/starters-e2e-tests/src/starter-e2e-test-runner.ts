import { fileURLToPath } from "url";
import { $, chalk, fs, path, spinner, tmpdir, within } from "zx";
import { WaspCliCommand } from "./cli.js";
import { setupWaspMailCrabConfiguration } from "./mailcrab.js";
import { StarterName } from "./starters.js";

interface StarterTestConfiguration {
  waspCliCommand: WaspCliCommand;
  starterName: StarterName;
}

export async function runStarterE2ETests(
  configuration: StarterTestConfiguration,
): Promise<void> {
  const waspProjectPath = await spinner(
    "Initializing test environment...",
    () => initializeTestEnvironment(configuration),
  );

  await runDevE2ETests(waspProjectPath, configuration);
  await runBuildE2ETests(waspProjectPath, configuration);
}

async function initializeTestEnvironment({
  waspCliCommand,
  starterName,
}: StarterTestConfiguration): Promise<string> {
  const tempDirectoryPath = tmpdir();
  process.on("exit", () => cleanup(tempDirectoryPath));
  process.on("SIGINT", () => cleanup(tempDirectoryPath));
  process.on("SIGTERM", () => cleanup(tempDirectoryPath));
  process.on("SIGQUIT", () => cleanup(tempDirectoryPath));

  const waspStarterProjectName = `wasp-starter-${starterName}`;

  await $({
    cwd: tempDirectoryPath,
  })`${waspCliCommand} new ${waspStarterProjectName} -t ${starterName}`;

  const waspProjectPath = path.join(tempDirectoryPath, waspStarterProjectName);

  await initializeServerEnvironment(waspProjectPath);
  await initializeClientEnvironment(waspProjectPath);
  await setupWaspMailCrabConfiguration(waspProjectPath);

  return waspProjectPath;
}

async function cleanup(tempDirectoryPath: string): Promise<void> {
  await $({ nothrow: true })`rm -rf ${tempDirectoryPath}`;
}

async function initializeServerEnvironment(
  waspProjectPath: string,
): Promise<void> {
  const serverEnvFileExamplePath = path.join(
    waspProjectPath,
    ".env.server.example",
  );
  const serverEnvFilePath = path.join(waspProjectPath, ".env.server");

  if (await fs.pathExists(serverEnvFileExamplePath)) {
    await fs.copy(serverEnvFileExamplePath, serverEnvFilePath);
  } else {
    await fs.ensureFile(serverEnvFilePath);
  }

  await fs.appendFile(
    serverEnvFilePath,
    "\nSKIP_EMAIL_VERIFICATION_IN_DEV=true\n",
  );
}

async function initializeClientEnvironment(
  waspProjectPath: string,
): Promise<void> {
  const clientEnvFileExamplePath = path.join(
    waspProjectPath,
    ".env.client.example",
  );
  const clientEnvFilePath = path.join(waspProjectPath, ".env.client");

  if (await fs.pathExists(clientEnvFileExamplePath)) {
    await fs.copy(clientEnvFileExamplePath, clientEnvFilePath);
  }
}

async function runDevE2ETests(
  waspProjectPath: string,
  { starterName, waspCliCommand }: StarterTestConfiguration,
): Promise<void> {
  console.log(`Running DEV e2e tests for ${starterName} starter...`);
  const waspAppRunnerDevEnv = {
    ...process.env,
    WASP_APP_PATH: waspProjectPath,
    WASP_CLI_CMD: waspCliCommand,
    WASP_RUN_MODE: "dev",
  };
  await $({
    env: waspAppRunnerDevEnv,
    stdio: "inherit",
  })`npx playwright test --grep ${getStarterPlaywrightGrepRegex(starterName)}`;
}

async function runBuildE2ETests(
  waspProjectPath: string,
  { starterName, waspCliCommand }: StarterTestConfiguration,
): Promise<void> {
  if (await waspProjectUsesSqlite(waspProjectPath, waspCliCommand)) {
    console.log(
      chalk.yellow(
        `Skipping BUILD e2e tests for ${starterName} starter - uses SQLite`,
      ),
    );
    return;
  }

  console.log(`Running BUILD e2e tests for ${starterName} starter...`);
  const waspAppRunnerBuildEnv = {
    ...process.env,
    WASP_APP_PATH: waspProjectPath,
    WASP_CLI_CMD: waspCliCommand,
    WASP_RUN_MODE: "build",
  };
  await $({
    env: waspAppRunnerBuildEnv,
    stdio: "inherit",
  })`npx playwright test --grep ${getStarterPlaywrightGrepRegex(starterName)}`;
}

/**
 * We search for Playwright tests with either:
 *  - tag for that starter template
 *  - no tag at all
 */
function getStarterPlaywrightGrepRegex(starterName: string): string {
  return `(@${starterName}|^(?!.*@).*)`;
}

async function waspProjectUsesSqlite(
  tempWaspProjectPath: string,
  waspCliCommand: string,
): Promise<boolean> {
  const scriptDirectory = path.dirname(fileURLToPath(import.meta.url));
  const databaseProviderScriptPath = path.join(
    scriptDirectory,
    "../../../../scripts/get-wasp-database-provider.sh",
  );

  return await within(async () => {
    $.cwd = tempWaspProjectPath;
    const result = await $`${databaseProviderScriptPath} ${waspCliCommand}`;
    return result.stdout.trim() === "sqlite";
  });
}
