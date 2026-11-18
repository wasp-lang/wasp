import { exec } from "child_process";

import { createLogger } from "./logging.js";

export async function checkDependencies() {
  const logger = createLogger("check-dependencies");
  const requiredCommands = ["docker"];

  for (const cmd of requiredCommands) {
    if (!(await commandExists(cmd))) {
      logger.error(`Required command '${cmd}' not found. Please install it.`);
      process.exit(1);
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
