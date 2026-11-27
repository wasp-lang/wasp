// Runs the Wasp binary of this `@wasp/cli`. Other `@wasp/cli` versions hand
// over to this one through `runWaspCli` (see the note in `package.json`).

import packageJson from "#/package.json" with { type: "json" };
import { execFileSync } from "node:child_process";
import { CLIError } from "./cli-error.js";
import { debug } from "./debug.js";
import { loadSubPackageForCurrentPlatform } from "./load-sub-package.js";

/** Reports an error instead of throwing it, and sets a failure exit code. */
export async function runAsExecutable(
  run: () => Promise<number>,
): Promise<void> {
  try {
    const exitCode = await run();
    debug("Wasp process exited with code: %d", exitCode);
    process.exitCode = exitCode;
  } catch (error) {
    debug("Error in launching Wasp: %o", error);
    // An error happened while trying to find or run the Wasp executable.
    // Report it and exit with an unknown failure error code.
    // Errors inside the Wasp process itself are not in this wrapper, as they are
    // handled by mimicking its exit code above.
    CLIError.log(error);
    process.exitCode = -1;
  } finally {
    debug("Exiting Wasp wrapper process with code: %d", process.exitCode);
  }
}

/** Returns the Wasp binary's exit code. */
export async function runWaspCli(): Promise<number> {
  const subPackage = await loadSubPackageForCurrentPlatform().catch(
    CLIError.rethrowWith("Can't find the Wasp executable."),
  );

  try {
    return runWaspBinary(
      subPackage.waspBinPath,
      process.argv.slice(2),
      makeWaspBinaryEnv(
        process.env,
        subPackage.dataDirPath,
        packageJson.version,
      ),
    );
  } catch (error: unknown) {
    throw new CLIError("Failed to run Wasp.", { cause: error });
  }
}

function makeWaspBinaryEnv(
  baseEnv: NodeJS.ProcessEnv,
  dataDirPath: string,
  waspVersion: string,
): NodeJS.ProcessEnv {
  return {
    ...baseEnv,
    waspc_datadir: dataDirPath,
    // Cabal versions can't express npm prereleases like `0.27.0-rc.1`, so
    // the binary takes its version from here.
    WASP_CLI_NPM_PACKAGE_VERSION: waspVersion,
  };
}

/** Returns the binary's exit code. Throws if the binary couldn't run. */
function runWaspBinary(
  waspBinPath: string,
  args: string[],
  env: NodeJS.ProcessEnv,
): number {
  debug("Running Wasp executable at path: %s", waspBinPath);
  debug("Using data directory at path: %s", env.waspc_datadir);
  debug("Passing arguments: %j", args);

  try {
    execFileSync(waspBinPath, args, { env, stdio: "inherit" });
    return 0;
  } catch (error: unknown) {
    const exitCode = getExitCode(error);
    if (exitCode === undefined) {
      throw error;
    }
    // The executed process ran correctly, but exited with a non-zero status
    // code. This is not considered an error in the scope of this wrapper,
    // so we just return the exit code.
    debug("We got an exit code from Wasp: %d", exitCode);
    return exitCode;
  }
}

/** The exit code of the process that `execFileSync` ran, if it got to exit. */
function getExitCode(execError: unknown): number | undefined {
  if (
    execError instanceof Error &&
    "status" in execError &&
    // The documentation points to this property being `number | undefined`,
    // but from testing it seems it can also be `null`.
    typeof execError.status === "number"
  ) {
    return execError.status;
  }
  return undefined;
}
