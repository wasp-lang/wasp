import { exec } from "child_process";

import { log } from "./logging.js";

export async function checkDependencies() {
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

function commandExists(command: string): Promise<boolean> {
  return new Promise((resolve) => {
    exec(`command -v ${command}`, (error) => {
      resolve(!error);
    });
  });
}
