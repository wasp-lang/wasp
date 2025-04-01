import { exec } from "child_process";

import { log } from "./logging.js";
import { installWaspCli } from "./waspCli.js";

export async function checkAndSetupDependencies({
  buildWaspCli,
}: {
  buildWaspCli: boolean;
}) {
  const requiredCommands = ["docker"];
  if (buildWaspCli) {
    requiredCommands.push("cabal");
  }

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

  if (buildWaspCli) {
    await installWaspCli();
  }
}

function commandExists(command: string): Promise<boolean> {
  return new Promise((resolve) => {
    exec(`command -v ${command}`, (error) => {
      resolve(!error);
    });
  });
}
