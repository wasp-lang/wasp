import { exec, spawn as _spawn } from "child_process";
import readline from "linebyline";
import yargs from "yargs/yargs";
import { hideBin } from "yargs/helpers";
import { join } from "path";
import { existsSync, copyFileSync } from "fs";

// Parse command line arguments
const argv = yargs(hideBin(process.argv))
  .option("app-path", {
    type: "string",
    description: "Path to the application",
    demandOption: true,
  })
  .option("app-name", {
    type: "string",
    description: "Name of the application (used for DB container name)",
    demandOption: true,
  })
  .option("db-type", {
    type: "choices",
    choices: ["sqlite", "postgres"],
    description: "Database type",
    default: "postgres",
  })
  .parse();

const pathToApp = argv["app-path"];
const appName = argv["app-name"];
const isPostgresUsed = argv["db-type"] === "postgres";
const children = [];
const POSTGRES_CONFIG = {
  port: 5432,
  password: "devpass",
  image: "postgres:16",
  healthCheckRetries: 10,
  healthCheckDelay: 2000,
  containerName: `${appName}-postgres`,
};

// Configure cleanup handlers
setupProcessCleanup();

// Improved logging with timestamps and colors
function log(processName, type, message) {
  const typeColor =
    {
      error: 31,
      warn: 33,
      info: 36,
      success: 32,
      debug: 90,
    }[type] || 0;

  console.log(
    `\x1b[0m` +
      `\x1b[${typeColor}m[${processName}${
        type ? `:${type}` : ""
      }]\x1b[0m ${message}`
  );
}

// Process cleanup handling
function setupProcessCleanup() {
  const cleanExit = (signal) => {
    log("shutdown", "warn", `Received ${signal}. Cleaning up...`);
    children.forEach((child) => {
      if (!child.killed) {
        child.kill();
      }
    });
    process.exit();
  };

  process.on("SIGINT", () => cleanExit("SIGINT"));
  process.on("SIGTERM", () => cleanExit("SIGTERM"));
}

// Validate required commands exist
async function checkDependencies() {
  const requiredCommands = ["docker", "cabal"];

  for (const cmd of requiredCommands) {
    if (!(await commandExists(cmd))) {
      log(
        "setup",
        "error",
        `Required command '${cmd}' not found. Please install it.`
      );
      process.exit(1);
    }
  }
}

function commandExists(command) {
  return new Promise((resolve) => {
    exec(`command -v ${command}`, (error) => {
      resolve(!error);
    });
  });
}

async function ensurePostgresContainer() {
  const { containerName, port, password, image } = POSTGRES_CONFIG;
  const DATABASE_URL = `postgresql://postgres:${password}@localhost:${port}/postgres`;

  try {
    // Validate application name format
    const validNameRegex = /^[a-zA-Z0-9][a-zA-Z0-9_.-]*$/;
    if (!validNameRegex.test(appName)) {
      throw new Error(
        `Invalid app name: ${appName}. It should only contain alphanumeric characters, dots, underscores, and dashes, and must start with an alphanumeric character.`
      );
    }

    // Check if container exists
    const exists = await dockerContainerExists(containerName);

    if (exists) {
      // Check if container is running
      const isRunning = await dockerContainerIsRunning(containerName);

      if (!isRunning) {
        log("postgres", "info", "Starting existing PostgreSQL container...");
        await dockerStartContainer(containerName);
      } else {
        log("postgres", "success", "PostgreSQL container already running");
      }
    } else {
      log("postgres", "info", "Creating new PostgreSQL container...");
      await dockerCreateContainer(containerName, port, password, image);
    }

    // Wait for PostgreSQL to become ready
    await waitForPostgresReady();

    return DATABASE_URL;
  } catch (error) {
    log("postgres", "error", error.message);
    process.exit(1);
  }
}

async function dockerContainerExists(containerName) {
  return new Promise((resolve) => {
    const proc = _spawn("docker", [
      "inspect",
      "--format={{.Name}}",
      containerName,
    ]);
    let output = "";

    proc.stdout.on("data", (data) => (output += data));
    proc.on("close", (code) => resolve(code === 0));
  });
}

async function dockerContainerIsRunning(containerName) {
  return new Promise((resolve) => {
    const proc = _spawn("docker", [
      "inspect",
      "--format={{.State.Running}}",
      containerName,
    ]);
    let output = "";

    proc.stdout.on("data", (data) => (output += data));
    proc.on("close", (code) => resolve(code === 0 && output.trim() === "true"));
  });
}

async function dockerStartContainer(containerName) {
  return new Promise((resolve, reject) => {
    const proc = _spawn("docker", ["start", containerName]);

    proc.on("close", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error("Failed to start container"));
      }
    });
  });
}

async function dockerCreateContainer(containerName, port, password, image) {
  return new Promise((resolve, reject) => {
    const args = [
      "run",
      "-d",
      "--name",
      containerName,
      "-p",
      `${port}:5432`,
      "-e",
      `POSTGRES_PASSWORD=${password}`,
      image,
    ];

    const proc = _spawn("docker", args);

    proc.on("close", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error("Failed to create container"));
      }
    });
  });
}

async function waitForPostgresReady() {
  const { containerName, healthCheckRetries, healthCheckDelay } =
    POSTGRES_CONFIG;

  for (let i = 1; i <= healthCheckRetries; i++) {
    log(
      "postgres",
      "info",
      `Checking PostgreSQL readiness (attempt ${i}/${healthCheckRetries})`
    );

    const success = await new Promise((resolve) => {
      const proc = _spawn("docker", [
        "exec",
        containerName,
        "pg_isready",
        "-U",
        "postgres",
      ]);

      proc.on("close", (code) => resolve(code === 0));
    });

    if (success) {
      log("postgres", "success", "PostgreSQL is ready");
      return;
    }

    await new Promise((r) => setTimeout(r, healthCheckDelay));
  }

  throw new Error("PostgreSQL did not become ready in time");
}

async function setupEnvFiles() {
  const envFiles = [
    { headless: ".env.client.headless", target: ".env.client" },
    { headless: ".env.server.headless", target: ".env.server" },
  ];

  envFiles.forEach(({ headless, target }) => {
    const headlessPath = join(pathToApp, headless);
    const targetPath = join(pathToApp, target);

    if (existsSync(headlessPath)) {
      log("setup", "info", `Copying ${headless} to ${target}`);
      copyFileSync(headlessPath, targetPath);
    } else {
      log("setup", "warn", `Headless env file not found: ${headless}`);
    }
  });
}

function spawn({ name, cmd, args, cwd, extraEnv = {} }) {
  return new Promise((resolve, reject) => {
    const spawnOptions = {
      cwd,
      env: { ...process.env, ...extraEnv },
      stdio: ["ignore", "pipe", "pipe"],
    };

    log(name, "info", `Spawning: ${cmd} ${args.join(" ")}`);

    const proc = _spawn(cmd, args, spawnOptions);
    children.push(proc);

    // Process output handling
    const handleStream = (stream, type) => {
      readline(stream).on("line", (line) => {
        log(name, type === "stderr" ? "error" : "info", line);
      });
    };

    handleStream(proc.stdout, "stdout");
    handleStream(proc.stderr, "stderr");

    proc.on("error", (err) => {
      log(name, "error", `Process error: ${err.message}`);
      if (err.code === "ENOENT") {
        log(name, "error", `Command '${cmd}' not found`);
      }
      reject(err);
    });

    proc.on("close", (code) => {
      children.splice(children.indexOf(proc), 1);
      if (code === 0) {
        log(name, "success", "Process completed successfully");
        resolve(code);
      } else {
        log(name, "error", `Process exited with code ${code}`);
        reject(code);
      }
    });
  });
}

async function installWaspCli() {
  log("install-wasp-cli", "info", "Installing Wasp CLI globally...");

  console.log(join(process.cwd(), "../waspc"));

  await spawn({
    name: "install-wasp-cli",
    cmd: "cabal",
    args: ["install", "--overwrite-policy=always"],
    cwd: join(__dirname, "../waspc"),
  });
}

async function main() {
  try {
    log("setup", "info", `Starting application: ${appName}`);

    await checkDependencies();

    let extraEnv = {};

    if (isPostgresUsed) {
      const DATABASE_URL = await ensurePostgresContainer();

      log("runApp", "info", `Using DATABASE_URL: ${DATABASE_URL}`);
      log(
        "runApp",
        "info",
        `Using DB container: ${POSTGRES_CONFIG.containerName}`
      );
      extraEnv = { DATABASE_URL };
    }

    // Add environment file setup
    await setupEnvFiles();

    await installWaspCli();

    await spawn({
      name: "migrate-db",
      cmd: "wasp-cli",
      args: ["db", "migrate-dev"],
      cwd: pathToApp,
      extraEnv,
    });

    await spawn({
      name: "start-app",
      cmd: "wasp-cli",
      args: ["start"],
      cwd: pathToApp,
      extraEnv,
    });
  } catch (error) {
    log("main", "error", `Fatal error: ${error.message}`);
    process.exit(1);
  }
}

main();
