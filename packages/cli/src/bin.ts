#!/usr/bin/env node

import { CLIError } from "./cli-error.js";
import { debug } from "./debug.js";
import { findProjectWaspCli } from "./find-project-wasp-cli.js";
import {
  getThisWaspCli,
  handOverTo,
  isSameWaspCli,
} from "./installed-wasp-cli.js";
import { runAsExecutable, runWaspCli } from "./run-wasp-cli.js";

await runAsExecutable(main);

async function main(): Promise<number> {
  const thisWaspCli = await getThisWaspCli();
  const projectWaspCli = await findProjectWaspCli(process.cwd());

  if (projectWaspCli === undefined) {
    return runWaspCli();
  }
  if (isSameWaspCli(projectWaspCli, thisWaspCli)) {
    debug("Running as the project's own Wasp CLI.");
    return runWaspCli();
  }

  debug(
    "Started by Wasp CLI %s, running the project's Wasp CLI %s.",
    thisWaspCli.version,
    projectWaspCli.version,
  );
  return handOverTo(projectWaspCli).catch(
    CLIError.rethrowWith("Failed to run the project's Wasp CLI."),
  );
}
