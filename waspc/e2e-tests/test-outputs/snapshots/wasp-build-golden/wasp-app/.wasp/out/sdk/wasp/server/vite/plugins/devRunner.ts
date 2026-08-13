import path from "node:path";
import {
  isRunnableDevEnvironment,
  type Plugin,
  type RunnableDevEnvironment,
  type ViteDevServer,
} from "vite";
import { ENVIRONMENT_NAMES } from "../../../vite/constants.js";

// PRIVATE API (used by the `wasp:server-env-file` plugin)
export const DEV_RUNNER_PLUGIN_NAME = "wasp:server-dev-runner";

// PRIVATE API (used by the `wasp:server-env-file` plugin)
export type ServerDevRunnerApi = {
  /**
   * Stops the running server (if there is one) and starts it again from
   * scratch.
   */
  restart: () => void;
};

/**
 * The module the dev runner imports to start the server.
 *
 * It is not the same file Wasp bundles for production: that one owns its
 * process and exits it when the server can't start, which would take the whole
 * dev server down with it.
 */
const serverStartFilePath = ".wasp/out/server/src/start.ts";

/**
 * Wasp regenerates a lot of files at once, so we wait for the burst of changes
 * to settle instead of restarting the server once per file.
 */
const restartDebounceMs = 150;

/**
 * When Vite restarts its dev server, it starts the new one while the old one is
 * still shutting down, so the server's port can stay taken for a moment.
 */
const portRetryCount = 10;
const portRetryDelayMs = 250;

/**
 * Runs the Wasp server inside the Vite dev server's process and restarts it
 * when its code changes.
 *
 * This is why there is a single process in development: the client and the
 * server share Vite's module graph, its file watcher and its terminal output.
 */
export function devRunner(): Plugin<ServerDevRunnerApi> {
  let viteDevServer: ViteDevServer | undefined;
  let serverHandle: ServerHandle | null = null;
  let restartTimeout: NodeJS.Timeout | undefined;
  /**
   * A server that failed to start leaves an incomplete module graph behind, so
   * Vite can't tell us which modules a change affected. While this is set, we
   * try to start the server again on any change.
   */
  let didLastStartFail = false;
  let isShuttingDown = false;

  /**
   * Rejects once Vite starts closing the dev server.
   *
   * Vite closes the channel the module runner fetches modules through as part
   * of closing the dev server, and it does so in parallel with the `closeBundle`
   * hook. An import that is waiting on that channel would never settle, so
   * importing has to race this promise. Otherwise stopping the server waits for
   * the import forever, and with it Vite's whole shutdown.
   */
  let beginShutdown!: () => void;
  const shutdown = new Promise<never>((_resolve, reject) => {
    beginShutdown = () =>
      reject(
        new Error("The Vite dev server closed while the server was starting."),
      );
  });
  // Nobody observes this promise unless an import is in flight, and an
  // unobserved rejection would look like an error nobody handles.
  void shutdown.catch(() => undefined);

  /**
   * Starting and stopping the server must never overlap. Without a queue, a
   * change arriving while the server is still starting (starting the job
   * executor takes seconds) orphans the instance that is starting: it finishes
   * booting, takes the port and starts its own job workers, while we believe
   * there is nothing to stop.
   */
  let queue: Promise<unknown> = Promise.resolve();
  function enqueue<T>(task: () => Promise<T>): Promise<T> {
    const taskResult = queue.then(task, task);
    queue = taskResult.catch(() => undefined);
    return taskResult;
  }

  function getServerRunner(): RunnableDevEnvironment["runner"] | undefined {
    const serverEnvironment =
      viteDevServer?.environments[ENVIRONMENT_NAMES.SERVER];
    if (!serverEnvironment || !isRunnableDevEnvironment(serverEnvironment)) {
      logError(
        `Wasp can't run the server because the "${ENVIRONMENT_NAMES.SERVER}" Vite environment is missing or can't run modules. Make sure your vite.config.ts uses the plugin from "wasp/server/vite" and doesn't override "environments.${ENVIRONMENT_NAMES.SERVER}.dev".`,
      );
      return undefined;
    }
    return serverEnvironment.runner;
  }

  async function start(): Promise<void> {
    // We only ever run the server for a dev server we attached to.
    if (isShuttingDown || !viteDevServer) {
      return;
    }

    const runner = getServerRunner();
    if (!runner) {
      didLastStartFail = true;
      return;
    }

    const startFileAbsPath = path.resolve(
      viteDevServer.config.root,
      serverStartFilePath,
    );

    try {
      serverHandle = await importAndStartServer(
        runner,
        startFileAbsPath,
        shutdown,
      );
      didLastStartFail = false;
    } catch (error) {
      if (isShuttingDown) {
        // We gave up on purpose, there is nothing to report.
        return;
      }
      didLastStartFail = true;
      logError("The server failed to start:", error);
      log("Fix the error above and save to try again.");
    }
  }

  async function stop(): Promise<void> {
    if (!serverHandle) {
      return;
    }
    // Forget the handle before closing so we never close the same server twice,
    // even if closing it fails.
    const handleToClose = serverHandle;
    serverHandle = null;
    try {
      await handleToClose.close();
    } catch (error) {
      logError("The server didn't stop cleanly:", error);
    }
  }

  function restart(): Promise<void> {
    return enqueue(async () => {
      await stop();
      // Forget the modules the previous server ran so the next start picks up
      // the new code, the user's code included.
      getServerRunner()?.clearCache();
      await start();
    });
  }

  function scheduleRestart(): void {
    clearTimeout(restartTimeout);
    restartTimeout = setTimeout(() => void restart(), restartDebounceMs);
  }

  return {
    name: DEV_RUNNER_PLUGIN_NAME,
    // Production servers run the bundle Wasp builds, they don't need a runner.
    apply: "serve",
    // Only the environment that runs the server needs this plugin's hooks.
    applyToEnvironment: (environment) =>
      environment.name === ENVIRONMENT_NAMES.SERVER,
    api: {
      restart: () => void restart(),
    },
    configureServer(server) {
      // Some Vite servers exist only to run a module once (the one
      // `wasp:validate-env` starts, for example). They have no HTTP server of
      // their own and they must not start a second copy of the app's server.
      const { httpServer } = server;
      if (server.config.server.middlewareMode || !httpServer) {
        return;
      }

      viteDevServer = server;
      installProcessGuards();

      httpServer.once("listening", () => void enqueue(start));
    },
    // `closeBundle` is Vite's teardown hook. The function a plugin can return
    // from `configureServer` looks like one but isn't: Vite calls it at startup,
    // right after it installs its own middlewares.
    //
    // Without stopping the server here, a Vite dev server restart (which
    // happens when, for example, `.env.client` changes) would leave the
    // previous server running and holding on to its port.
    async closeBundle() {
      // Vite reuses these plugin instances for the temporary server
      // `wasp:validate-env` starts, so this hook also runs when that server
      // closes. Closing it must not stop the server the user is running.
      if (
        this.environment !==
        viteDevServer?.environments[ENVIRONMENT_NAMES.SERVER]
      ) {
        return;
      }

      // Everything up to the first `await` runs before Vite can close anything
      // else, so this is where we make sure nothing tries to start again.
      isShuttingDown = true;
      clearTimeout(restartTimeout);
      beginShutdown();

      await enqueue(stop);
    },
    hotUpdate(context) {
      if (isShuttingDown || !viteDevServer) {
        return [];
      }
      if (context.modules.length > 0 || didLastStartFail) {
        scheduleRestart();
      }
      // Returning no modules tells Vite not to handle the update itself. It
      // only affects this environment, the client keeps its own HMR.
      return [];
    },
  };
}

type ServerHandle = {
  close: () => Promise<void>;
};

type ServerStartModule = {
  startServer: () => Promise<ServerHandle>;
};

async function importAndStartServer(
  runner: RunnableDevEnvironment["runner"],
  startFileAbsPath: string,
  shutdown: Promise<never>,
): Promise<ServerHandle> {
  for (let attempt = 1; ; attempt++) {
    try {
      const importedStartModule = runner.import(
        startFileAbsPath,
      ) as Promise<ServerStartModule>;
      // When the race below abandons this import, a failure it reports later
      // would look like an error nobody handles.
      void importedStartModule.catch(() => undefined);

      const startModule = await Promise.race([importedStartModule, shutdown]);
      return await startModule.startServer();
    } catch (error) {
      if (!isPortTakenError(error) || attempt >= portRetryCount) {
        throw error;
      }
      log(
        `The server's port is still taken, retrying (${attempt}/${portRetryCount})...`,
      );
      await delay(portRetryDelayMs);
    }
  }
}

/**
 * The server turns the error Node.js throws when a port is taken into a
 * friendlier one, so we have to walk the chain of causes to recognize it.
 */
function isPortTakenError(error: unknown): boolean {
  for (
    let cause: unknown = error;
    cause instanceof Error;
    cause = cause.cause
  ) {
    if ((cause as NodeJS.ErrnoException).code === "EADDRINUSE") {
      return true;
    }
  }
  return false;
}

function delay(milliseconds: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, milliseconds));
}

let areProcessGuardsInstalled = false;

/**
 * In development the user's server code shares its process with Vite, so an
 * error nobody handles would take the client's dev server down with it. Node.js
 * ends the process on those by default, so we have to handle them ourselves.
 */
function installProcessGuards(): void {
  if (areProcessGuardsInstalled) {
    return;
  }
  areProcessGuardsInstalled = true;

  process.on("unhandledRejection", (reason) => {
    logError("Unhandled rejection in server code:", reason);
    log("The dev server is still running, fix the error above and save.");
  });
  process.on("uncaughtException", (error) => {
    logError("Uncaught exception in server code:", error);
    log("The dev server is still running, fix the error above and save.");
  });
}

const logPrefix = "\x1b[35m[server]\x1b[0m";

/**
 * We log through `console` instead of Vite's logger because the logger belongs
 * to a config object, and the config the plugin sees isn't always the one of
 * the dev server the user is looking at (`wasp:validate-env` resolves a silent
 * config of its own).
 */
function log(message: string): void {
  console.log(`${logPrefix} ${message}`);
}

function logError(message: string, error?: unknown): void {
  console.error(`${logPrefix} ${message}`);
  if (error !== undefined) {
    console.error(error);
  }
}
