import { exec } from "child_process";

import { createLogger } from "./logging.js";

const logger = createLogger("check-dependencies");

export async function checkDependencies() {
  const requiredCommands = ["docker"];

  for (const cmd of requiredCommands) {
    if (!(await commandExists(cmd))) {
      throw logger.cliError(
        `Required command '${cmd}' not found. Please install it.`,
      );
    }
  }
}

function commandExists(command: string): Promise<boolean> {
  return new Promise((resolve) => {
    exec(`command -v ${command}`, (error) => {
      resolve(!error);
    });
  });
}
