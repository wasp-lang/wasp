{{={= =}=}}
import { parse as parseDotenv } from "dotenv";
import { expand, type DotenvPopulateInput } from "dotenv-expand";
import fs from "node:fs";
import path from "node:path";
import { type Plugin, type ViteDevServer } from "vite";
import {
  DEV_RUNNER_PLUGIN_NAME,
  type ServerDevRunnerApi,
} from "./devRunner.js";

/**
 * The env file Wasp generates for the server. It holds the user's
 * `.env.server` variables and, when Wasp manages the database, the URL of the
 * development database.
 */
const serverEnvFilePath = "{= serverEnvFilePath =}";

/**
 * Loads the server's env file into `process.env`.
 *
 * In development the server runs inside the Vite dev server's process, so it
 * reads its configuration from the same `process.env` Vite was started with.
 * In production the server gets its environment from wherever it is deployed.
 */
export function envFile(): Plugin {
  let envFileAbsPath!: string;

  return {
    name: "wasp:server-env-file",
    // Only the dev server runs the server in its own process.
    apply: "serve",
    // We load the file in `configResolved` because it runs after every `config`
    // hook. Loading it earlier could change the values the client plugins
    // inline into `import.meta.env`.
    configResolved(config) {
      envFileAbsPath = path.resolve(config.root, serverEnvFilePath);
      loadEnvFileIntoProcessEnv(envFileAbsPath);
      // Vite already does this for us, but the server's env schema depends on
      // it, so we make sure.
      process.env.NODE_ENV ??= "development";
    },
    configureServer(server) {
      const restartServerOnEnvFileEvent = (changedPath: string): void => {
        if (changedPath !== envFileAbsPath) {
          return;
        }
        loadEnvFileIntoProcessEnv(envFileAbsPath);
        // The server reads its environment when it starts, so it has to start
        // again. Restarting the whole Vite dev server would also reload the
        // user's browser for no reason.
        getDevRunnerApi(server)?.restart();
      };

      server.watcher.on("add", restartServerOnEnvFileEvent);
      server.watcher.on("change", restartServerOnEnvFileEvent);
      server.watcher.on("unlink", restartServerOnEnvFileEvent);
    },
  };
}

/**
 * The variables Vite was started with. They always win over the ones from the
 * env file, and we never remove them.
 *
 * This lives outside the plugin because Vite builds new plugin instances when
 * it restarts its dev server, and the variables we loaded before that restart
 * must not be mistaken for the process's own.
 */
let ambientEnvVarNames: ReadonlySet<string> | undefined;
let envVarNamesLoadedFromFile: string[] = [];

function loadEnvFileIntoProcessEnv(envFileAbsPath: string): void {
  ambientEnvVarNames ??= new Set(Object.keys(process.env));

  // Forget the values from the previous read so variables the user removed from
  // the file really disappear.
  for (const name of envVarNamesLoadedFromFile) {
    delete process.env[name];
  }
  envVarNamesLoadedFromFile = [];

  const parsed = parseEnvFile(envFileAbsPath);
  // Let the file's variables reference each other. We expand against a copy of
  // `process.env` so `dotenv-expand` doesn't write into it directly.
  expand({ parsed, processEnv: { ...process.env } as DotenvPopulateInput });

  for (const [name, value] of Object.entries(parsed)) {
    if (ambientEnvVarNames.has(name)) {
      continue;
    }
    process.env[name] = value;
    envVarNamesLoadedFromFile.push(name);
  }
}

function parseEnvFile(envFileAbsPath: string): Record<string, string> {
  let fileContents: string;
  try {
    fileContents = fs.readFileSync(envFileAbsPath, "utf-8");
  } catch {
    // Wasp doesn't generate the file when there is nothing to put in it.
    return {};
  }
  return parseDotenv(fileContents);
}

function getDevRunnerApi(
  server: ViteDevServer,
): ServerDevRunnerApi | undefined {
  const devRunnerPlugin = server.config.plugins.find(
    (plugin) => plugin.name === DEV_RUNNER_PLUGIN_NAME,
  );
  return devRunnerPlugin?.api as ServerDevRunnerApi | undefined;
}
