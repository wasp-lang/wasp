import { join } from "path";
import type { Options } from "./cli.js";
import { copyFileSync, existsSync } from "fs";
import { log } from "./logging.js";

export async function setupEnvFiles(options: Options) {
  const envFiles = [
    { headless: ".env.client.headless", target: ".env.client" },
    { headless: ".env.server.headless", target: ".env.server" },
  ];

  envFiles.forEach(({ headless, target }) => {
    const headlessPath = join(options.pathToApp, headless);
    const targetPath = join(options.pathToApp, target);

    if (existsSync(headlessPath)) {
      log("setup", "info", `Copying ${headless} to ${target}`);
      copyFileSync(headlessPath, targetPath);
    } else {
      log("setup", "warn", `Headless env file not found: ${headless}`);
    }
  });
}
