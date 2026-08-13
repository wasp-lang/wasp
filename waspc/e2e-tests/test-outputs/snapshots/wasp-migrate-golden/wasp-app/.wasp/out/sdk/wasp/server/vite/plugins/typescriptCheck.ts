import { spawn, type ChildProcess } from "node:child_process";
import path from "node:path";
import { type Plugin, type ViteDevServer } from "vite";
import { ENVIRONMENT_NAMES } from "../../../vite/constants.js";
import { log, logError } from "../logging.js";

/**
 * The user's TypeScript project: all the code they write, client and server
 * alike, checked with their own compiler options.
 *
 * This is the same project the client's `wasp:typescript-check` plugin checks
 * when Wasp builds the app for production. The code Wasp generates is checked
 * by the server's `bundle` script instead: checking it needs the user's
 * project to be built first, and building it would mean writing files that
 * nothing reads while the user develops.
 */
const srcTsConfigPath = "tsconfig.src.json";

/**
 * Wasp regenerates a lot of files at once, so we wait for the burst of changes
 * to settle instead of checking once per file. Same value as the dev runner's
 * restart debounce, so a change starts the check and the restart together.
 */
const checkDebounceMs = 150;

/**
 * TSC's incremental cache is the only file the check writes. Reacting to it
 * would make every check schedule the next one.
 */
const tscCacheFileExtension = ".tsbuildinfo";

/**
 * Type-checks the user's code while they develop it.
 *
 * Vite itself never type-checks: it strips the types away without looking at
 * them. This check runs next to the server instead of in front of it, because
 * types don't exist at runtime: a type error is worth reporting, but it
 * shouldn't stop the user from trying their app out.
 */
export function typescriptCheck(): Plugin {
  let viteDevServer: ViteDevServer | undefined;
  let runningCheck: ChildProcess | undefined;
  let checkTimeout: NodeJS.Timeout | undefined;
  let lastReportedErrors: string | undefined;

  function scheduleCheck(): void {
    clearTimeout(checkTimeout);
    checkTimeout = setTimeout(runCheck, checkDebounceMs);
  }

  /**
   * Whatever a running check is about to say is already out of date, and
   * letting checks pile up would leave one TypeScript process per change
   * behind.
   */
  function stopRunningCheck(): void {
    runningCheck?.kill();
    runningCheck = undefined;
  }

  function runCheck(): void {
    if (!viteDevServer) {
      return;
    }
    stopRunningCheck();

    const waspProjectDir = viteDevServer.config.root;
    const check = spawn(
      "tsc",
      [
        "--project",
        path.resolve(waspProjectDir, srcTsConfigPath),
        // Emitting would only slow the check down, and the files TSC writes
        // would look like changes worth checking again.
        "--noEmit",
        // One line per error, which is what we can prefix and forward.
        "--pretty",
        "false",
      ],
      {
        cwd: waspProjectDir,
        // We report TypeScript's output ourselves so it can't get mixed up
        // with the app's own output.
        stdio: ["ignore", "pipe", "pipe"],
        shell: process.platform === "win32",
      },
    );
    runningCheck = check;

    const outputChunks: string[] = [];
    const collectOutput = (chunk: unknown): void => {
      outputChunks.push(String(chunk));
    };
    check.stdout?.on("data", collectOutput);
    check.stderr?.on("data", collectOutput);

    check.once("error", (error) => {
      if (runningCheck !== check) {
        return;
      }
      runningCheck = undefined;
      logError("Wasp couldn't run TypeScript to check your code:", error);
    });

    check.once("close", (exitCode) => {
      // A check we stopped ourselves has nothing to say, a newer one is on its
      // way.
      if (runningCheck !== check) {
        return;
      }
      runningCheck = undefined;
      reportCheckResult(exitCode, outputChunks.join(""));
    });
  }

  function reportCheckResult(exitCode: number | null, output: string): void {
    if (exitCode === 0) {
      if (lastReportedErrors !== undefined) {
        log("No more TypeScript errors.");
        lastReportedErrors = undefined;
      }
      return;
    }
    // One change to the user's code makes Wasp regenerate code and rebuild the
    // SDK, so a single save runs the check a few times. Saying the same thing
    // over and over would only make the errors harder to read.
    if (output === lastReportedErrors) {
      return;
    }
    lastReportedErrors = output;

    const outputLines = output
      .split("\n")
      .map((line) => line.trimEnd())
      .filter((line) => line !== "");
    if (outputLines.length === 0) {
      logError(
        `TypeScript stopped with exit code ${exitCode} and said nothing.`,
      );
      return;
    }

    logError("TypeScript found errors in your code:");
    for (const line of outputLines) {
      logError(line);
    }
    log("Your app keeps running, but `wasp build` fails until you fix them.");
  }

  return {
    name: "wasp:dev-typescript-check",
    // Production builds check the user's code through the
    // `wasp:typescript-check` plugin and the generated code through the
    // server's `bundle` script.
    apply: "serve",
    // The check covers the whole app, so it must run once per change, not once
    // per environment.
    applyToEnvironment: (environment) =>
      environment.name === ENVIRONMENT_NAMES.SERVER,
    configureServer(server) {
      // Some Vite servers exist only to run a module once (the one
      // `wasp:validate-env` starts, for example). Nobody watches their output
      // and they never see a file change.
      const { httpServer } = server;
      if (server.config.server.middlewareMode || !httpServer) {
        return;
      }

      viteDevServer = server;

      httpServer.once("listening", scheduleCheck);
    },
    hotUpdate({ file }) {
      if (file.endsWith(tscCacheFileExtension)) {
        return;
      }
      // Every environment gets this hook for every changed file, so we ignore
      // the modules Vite matched: a type error in code the server never
      // imports (a React component the user hasn't opened yet, for example) is
      // still an error in their app.
      scheduleCheck();
      // Returning nothing leaves the update to the plugins that handle it.
    },
    // `closeBundle` is Vite's teardown hook. Without it, a Vite dev server
    // restart (which happens when, for example, `.env.client` changes) would
    // leave the running check behind.
    closeBundle() {
      // Vite reuses these plugin instances for the temporary server
      // `wasp:validate-env` starts, so this hook also runs when that server
      // closes.
      if (
        this.environment !==
        viteDevServer?.environments[ENVIRONMENT_NAMES.SERVER]
      ) {
        return;
      }

      clearTimeout(checkTimeout);
      stopRunningCheck();
    },
  };
}
