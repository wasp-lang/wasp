#!/usr/bin/env node

import { CLIError } from "./cli-error.js";
import { debug } from "./debug.js";
import {
  findProjectWaspCliToHandOverTo,
  getThisWaspCli,
  runProjectWaspCli,
} from "./project-wasp-cli.js";
import { runWaspCli } from "./run.js";

try {
  const childProcessExitCode = await main();
  debug("Wasp process exited with code: %d", childProcessExitCode);
  process.exitCode = childProcessExitCode;
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

async function main(): Promise<number> {
  const thisWaspCli = await getThisWaspCli();

  const projectWaspCli = await findProjectWaspCliToHandOverTo(thisWaspCli);
  if (projectWaspCli !== undefined) {
    return runProjectWaspCli(projectWaspCli, thisWaspCli).catch(
      CLIError.rethrowWith("Failed to run the project's Wasp CLI."),
    );
  } else {
    return runWaspCli();
  }
}
